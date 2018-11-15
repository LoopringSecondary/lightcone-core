/*
 * Copyright 2018 Loopring Foundation
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.loopring.lightcone.core.market

import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.depth._
import org.loopring.lightcone.core.base._

import org.slf4s.Logging
import scala.annotation.tailrec
import scala.collection.mutable.{ SortedSet, Map }

object MarketManagerImpl {
  private def defaultOrdering() = new Ordering[Order] {
    def compare(a: Order, b: Order) = {
      if (a.rate < b.rate) -1
      else if (a.rate > b.rate) 1
      else if (a.createdAt < b.createdAt) -1
      else if (a.createdAt > b.createdAt) 1
      else a.id compare b.id //在rate和createAt相同时，根据id排序，否则会丢单
    }
  }
}

class MarketManagerImpl(
    val marketId: MarketId,
    val config: MarketManagerConfig,
    val tokenMetadataManager: TokenMetadataManager,
    val ringMatcher: RingMatcher,
    val pendingRingPool: PendingRingPool,
    val dustOrderEvaluator: DustOrderEvaluator
) extends MarketManager with Logging {

  import MarketManagerImpl._
  import MatchingFailure._
  import OrderStatus._

  private implicit val marketId_ = marketId
  private implicit val tmm_ = tokenMetadataManager
  private implicit val ordering = defaultOrdering()

  private var isLastTakerSell = false
  private var lastPrice: Double = 0

  private[core] val bids = SortedSet.empty[Order] // order.tokenS == marketId.primary
  private[core] val asks = SortedSet.empty[Order] // order.tokenS == marketId.secondary

  private[core] val orderMap = Map.empty[String, Order]
  val aggregator = new OrderAwareOrderbookAggregator(config.priceDecimals)

  private[core] val sides = Map(
    marketId.primary -> bids,
    marketId.secondary -> asks
  )

  def getOrder(orderId: String) = orderMap.get(orderId).map(updateOrderMatchable)

  def submitOrder(order: Order, minFiatValue: Double = 0): MatchResult = {
    // Allow re-submission of an existing order.
    removeFromSide(order.id)
    matchOrders(order, minFiatValue)
  }

  def deleteOrder(orderId: String): OrderbookUpdate = {
    removeFromSide(orderId)
    pendingRingPool.deleteOrder(orderId)
    aggregator.getOrderbookUpdate()
  }

  def deletePendingRing(ringId: String): OrderbookUpdate = {
    pendingRingPool.deleteRing(ringId)
    aggregator.getOrderbookUpdate()
  }

  def triggerMatch(
    sellOrderAsTaker: Boolean,
    minFiatValue: Double = 0,
    offset: Int = 0
  ): Option[MatchResult] = {
    val side = if (sellOrderAsTaker) asks else bids
    val takerOption = side.drop(offset).headOption
    takerOption.map(submitOrder(_, minFiatValue))
  }

  private[core] def matchOrders(order: Order, minFiatValue: Double): MatchResult = {
    if (dustOrderEvaluator.isOriginalDust(order)) {
      MatchResult(Nil, order.copy(status = DUST_ORDER), OrderbookUpdate(Nil, Nil))

    } else if (dustOrderEvaluator.isActualDust(order)) {
      MatchResult(Nil, order.copy(status = COMPLETELY_FILLED), OrderbookUpdate(Nil, Nil))

    } else {
      var taker = order.copy(status = PENDING)
      var rings = Seq.empty[OrderRing]
      var ordersToAddBack = Seq.empty[Order]

      // The result of this recursive method is to populate
      // `rings` and `ordersToAddBack`.
      @tailrec
      def recursivelyMatchOrders(): Unit = {
        taker = updateOrderMatchable(order)
        if (dustOrderEvaluator.isMatchableDust(taker)) return

        popBestMakerOrder(taker).map { order ⇒
          val maker = updateOrderMatchable(order)
          val matchResult =
            if (dustOrderEvaluator.isMatchableDust(maker)) Left(INCOME_TOO_SMALL)
            else ringMatcher.matchOrders(taker, maker, minFiatValue)

          log.debug(
            s"""
            recursively match orders ===>
            - taker: $taker,
            - maker: $maker,
            - matchResult: $matchResult
            """
          )
          (maker, matchResult)
        } match {
          case None ⇒ // to maker to trade with
          case Some((maker, matchResult)) ⇒
            // we alsways need to add maker back even if it is PENDING-fully-matched.
            ordersToAddBack +:= maker
            matchResult match {
              case Left(ORDERS_NOT_TRADABLE) ⇒

              case Left(_) ⇒
                recursivelyMatchOrders()

              case Right(ring) ⇒
                isLastTakerSell = (taker.tokenS == marketId.secondary)
                rings +:= ring
                pendingRingPool.addRing(ring)
                recursivelyMatchOrders()
            }
        }
      }

      recursivelyMatchOrders()

      // we alsways need to add the taker back even if it is PENDING-fully-matched.
      ordersToAddBack +:= taker

      // add each skipped maker orders back
      ordersToAddBack.map(_.resetMatchable).foreach(addToSide)

      MatchResult(
        rings,
        taker.resetMatchable,
        aggregator.getOrderbookUpdate()
      )
    }
  }

  // TODO(dongw)
  def getMetadata() = MarketMetadata(
    numBuys = bids.size,
    numSells = asks.size,
    numHiddenBuys = 0,
    numHiddenSells = 0,
    bestBuyPrice = 0.0,
    bestSellPrice = 0.0,
    lastPrice = 0.0,
    isLastTakerSell = isLastTakerSell
  )

  // Add an order to its side.
  private def addToSide(order: Order) {
    // always make sure _matchable is None.
    val order_ = order.copy(_matchable = None)
    aggregator.addOrder(order)
    orderMap += order.id -> order_
    sides(order.tokenS) += order_
  }

  private def removeFromSide(orderId: String) {
    orderMap.get(orderId) match {
      case None ⇒
      case Some(order) ⇒
        aggregator.deleteOrder(order)
        orderMap -= order.id
        sides(order.tokenS) -= order
    }
  }

  // Remove and return the top taker order for a taker order.
  private def popBestMakerOrder(order: Order): Option[Order] =
    popOrder(sides(order.tokenB))

  // Remove and return the top order from one side.
  private def popOrder(side: SortedSet[Order]): Option[Order] = {
    side.headOption.map { order ⇒
      aggregator.deleteOrder(order)
      orderMap -= order.id
      side -= order
      order
    }
  }

  private def updateOrderMatchable(order: Order): Order = {
    val pendingAmountS = pendingRingPool.getOrderPendingAmountS(order.id)
    val matchableAmountS = (order.actual.amountS - pendingAmountS).max(0)
    val scale = Rational(matchableAmountS, order.original.amountS)
    order.copy(_matchable = Some(order.original.scaleBy(scale)))
  }
}
