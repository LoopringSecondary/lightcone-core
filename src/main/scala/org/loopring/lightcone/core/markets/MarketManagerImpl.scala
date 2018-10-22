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

import scala.collection.mutable.SortedSet
import org.slf4s.Logging
import scala.annotation.tailrec

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
    config: MarketManagerConfig
)(
    implicit
    pendingRingPool: PendingRingPool,
    orderPool: OrderPool
) extends MarketManager with Logging {
  import MarketManagerImpl._

  private implicit val ordering = defaultOrdering()
  private[core] val bids = SortedSet.empty[Order]
  private[core] val bidsRecyclable = SortedSet.empty[Order]
  private[core] val asks = SortedSet.empty[Order]
  private[core] val asksRecyclable = SortedSet.empty[Order]

  private val sides = Map(marketId.primary -> bids, marketId.secondary -> asks)

  def addOrder(order: Order): Seq[Ring] = {
    log.debug(s"taker order: $order")
    var rings = Seq.empty[Ring]

    case class State(taker: Order, pending: OrderState = OrderState())

    @tailrec
    def recursivelyMatchOrder(state: State): State = {

      recursivelyMatchOrder(state.copy(pending = OrderState()))
    }

    val State(updatedOrder, pending) = recursivelyMatchOrder(State(order))

    rings
  }

  private def getFirstOrder(orders: SortedSet[Order]): Option[Order] = {
    var matchableAmountS: Amount = 0

    def filterMethod(order: Order) = {
      val pendingAmountS = pendingRingPool.getOrderPendingAmountS(order.id)
      matchableAmountS = order.actual.amountS - pendingAmountS
      matchableAmountS > 0
    }

    orders.collectFirst {
      case order: Order if filterMethod(order) ⇒
        val r = Rational(matchableAmountS, order.amountS)

        val updatedOrder = order.copy(_matchable = Some(OrderState(
          matchableAmountS,
          (r * Rational(order.amountB)).bigintValue,
          (r * Rational(order.amountFee)).bigintValue
        )))

        log.debug(s"top maker order: $updatedOrder")
        updatedOrder
    }
  }
}
