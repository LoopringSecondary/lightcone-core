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

package org.loopring.lightcone.core

import org.loopring.lightcone.core.markets.DustEvaluator
import org.slf4s.Logging
import scala.annotation.tailrec
import scala.collection.mutable.SortedSet

// For ABC-XYZ market, ABC is secondary, XYZ is primary
case class MarketId(
    primary: Address,
    secondary: Address
)

case class MarketManagerConfig(
    maxNumBuys: Int,
    maxNumSells: Int,
    maxNumHiddenBuys: Int,
    maxNumHiddenSells: Int
)

object MarketManagerImpl {
  private def defaultOrdering() = new Ordering[Order] {
    def compare(a: Order, b: Order) = {
      if (a.rate < b.rate) -1
      else if (a.rate > b.rate) 1
      else if (a.createdAt < b.createdAt) -1
      else if (a.createdAt > b.createdAt) 1
      else 0
    }
  }
}

class MarketManagerImpl(
    marketId: MarketId,
    config: MarketManagerConfig,
    ringMatcher: RingMatcher,
    dustEvaluator: DustEvaluator
)(
    implicit
    pendingRingPool: PendingRingPool,
    orderPool: OrderPool
) extends MarketManager with Logging {

  import MarketManagerImpl._
  import MatchingFailure._

  private implicit val ordering = defaultOrdering()
  private[core] val bids = SortedSet.empty[Order]
  private[core] val asks = SortedSet.empty[Order]

  private val sides = Map(marketId.primary -> bids, marketId.secondary -> asks)

  def submitOrder(order: Order): SubmitOrderResult = {
    log.debug(s"taker order: $order")

    var rings = Seq.empty[Ring]
    var skippedOrders = Seq.empty[Order]
    var fullyMatchedOrderIds = Seq.empty[ID]
    var taker = order

    recursivelyMatchOrder()

    @tailrec
    def recursivelyMatchOrder(): Unit = {
      val matchResult = for {
        taker ← Some(taker)
        maker ← takeTopMaker(taker)
      } yield (maker, ringMatcher.matchOrders(taker, maker))

      matchResult match {
        case Some((maker, Left(failure))) ⇒ failure match {
          case ORDERS_NOT_TRADABLE ⇒
            skippedOrders :+= maker

          case INCOME_TOO_SMALL ⇒
            skippedOrders :+= maker
            recursivelyMatchOrder()
        }

        case Some((maker, Right(ring))) ⇒
          rings :+= ring

          taker = ring.taker.order
          val updatedMaker = ring.maker.order

          if (dustEvaluator.isDust(updatedMaker)) {
            fullyMatchedOrderIds :+= updatedMaker.id
          } else {
            skippedOrders :+= updatedMaker
          }

          if (dustEvaluator.isDust(taker)) {
            fullyMatchedOrderIds :+= taker.id
          } else {
            recursivelyMatchOrder()
          }

        case None ⇒
          if (!dustEvaluator.isDust(taker)) {
            skippedOrders :+= taker
          }
      }
    }

    // TODO(hongyu): choose side
    val side = bids // or ask
    skippedOrders.foreach(side += _)

    SubmitOrderResult(rings, fullyMatchedOrderIds)
  }

  private def takeTopMaker(order: Order): Option[Order] = {
    None
    // var matchableAmountS: Amount = 0

    // def filterMethod(order: Order) = {
    //   val pendingAmountS = pendingRingPool.getOrderPendingAmountS(order.id)
    //   matchableAmountS = order.actual.amountS - pendingAmountS
    //   matchableAmountS > 0
    // }

    // orders.collectFirst {
    //   case order: Order if filterMethod(order) ⇒
    //     val r = Rational(matchableAmountS, order.amountS)

    //     val updatedOrder = order.copy(_matchable = Some(OrderState(
    //       matchableAmountS,
    //       (r * Rational(order.amountB)).bigintValue,
    //       (r * Rational(order.amountFee)).bigintValue
    //     )))

    //     log.debug(s"top maker order: $updatedOrder")
    //     updatedOrder
    // }
  }

}
