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
      else a.id compare b.id //在rate和createAt相同时，根据id排序，否则会丢单
    }
  }
}

class MarketManagerImpl(
    marketId: MarketId,
    config: MarketManagerConfig,
    ringMatcher: RingMatcher
)(
    implicit
    pendingRingPool: PendingRingPool,
    orderPool: OrderPool,
    dustOrderEvaluator: DustOrderEvaluator
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
    var makerOrdersRecyclable = Seq.empty[Order]
    var fullyMatchedOrderIds = Seq.empty[ID]

    val subedPendingAmountS =
      order.matchable.amountS -
        pendingRingPool.getOrderPendingAmountS(order.id)

    var taker = order.copy(_matchable = Some(OrderState(
      amountS = subedPendingAmountS,
      amountB = Rational(
        subedPendingAmountS * order.amountB,
        order.amountS
      ).bigintValue(),
      amountFee = Rational(
        subedPendingAmountS * order.amountFee,
        order.amountS
      ).bigintValue()
    )))

    if (dustOrderEvaluator.isDust(taker)) {
      fullyMatchedOrderIds :+= taker.id
      return SubmitOrderResult(rings, fullyMatchedOrderIds)
    }

    @tailrec
    def recursivelyMatchOrder(): Unit = {
      val matchResult = for {
        maker ← takeTopMaker(taker)
      } yield (maker, ringMatcher.matchOrders(taker, maker))

      matchResult match {
        case Some((maker, Left(failure))) ⇒ failure match {
          case ORDERS_NOT_TRADABLE ⇒
            makerOrdersRecyclable :+= maker

          case INCOME_TOO_SMALL ⇒
            makerOrdersRecyclable :+= maker
            recursivelyMatchOrder()
        }

        case Some((maker, Right(ring))) ⇒
          rings :+= ring

          taker = ring.taker.order
          val updatedMaker = ring.maker.order

          if (dustOrderEvaluator.isDust(updatedMaker)) {
            fullyMatchedOrderIds :+= updatedMaker.id
          } else {
            makerOrdersRecyclable :+= updatedMaker
          }

          if (dustOrderEvaluator.isDust(taker)) {
            fullyMatchedOrderIds :+= taker.id
          } else {
            recursivelyMatchOrder()
          }

        case None ⇒
          if (!dustOrderEvaluator.isDust(taker)) {
            addToSide(taker)
          }
      }
    }

    recursivelyMatchOrder()

    makerOrdersRecyclable.foreach(addToSide)

    rings.foreach(pendingRingPool.addRing)

    SubmitOrderResult(rings, fullyMatchedOrderIds)
  }

  private def addToSide(order: Order) = {
    sides(order.tokenS).add(order)
  }

  private def takeTopMaker(order: Order): Option[Order] = {
    val orders = sides(order.tokenB)

    orders.headOption.map {
      head ⇒
        orders.remove(head)
        head
    }

  }

}
