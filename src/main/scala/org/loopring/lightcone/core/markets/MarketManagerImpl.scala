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
  private def defaultOrdering[T]() = new Ordering[Order[T]] {
    def compare(a: Order[T], b: Order[T]) = {
      if (a.rate < b.rate) -1
      else if (a.rate > b.rate) 1
      else if (a.createdAt < b.createdAt) -1
      else if (a.createdAt > b.createdAt) 1
      else 0
    }
  }
}

class MarketManagerImpl[T](
    marketId: MarketId,
    config: MarketManagerConfig
)(
    implicit
    pendingRingPool: PendingRingPool[T],
    orderPool: OrderPool[T]
) extends MarketManager[T] with Logging {
  import MarketManagerImpl._

  private implicit val ordering = defaultOrdering[T]()
  private[core] val bids = SortedSet.empty[Order[T]]
  private[core] val asks = SortedSet.empty[Order[T]]

  private val sides = Map(marketId.primary -> bids, marketId.secondary -> asks)

  def addOrder(order: Order[T]): Seq[Ring[T]] = {
    log.debug(s"taker order: $order")
    var rings = Seq.empty[Ring[T]]

    case class State[T](taker: Order[T], pending: OrderState = OrderState())

    def recursivelyMatchOrder(state: State[T]): State[T] = {
      recursivelyMatchOrder(state)
    }

    val State(updatedOrder, pending) = recursivelyMatchOrder(State[T](order))

    rings
  }

  private def getFirstOrder(orders: SortedSet[Order[T]]): Option[Order[T]] = {
    var matchableAmountS: Amount = 0

    def filterMethod(order: Order[T]) = {
      val pendingAmountS = pendingRingPool.getOrderPendingAmountS(order.id)
      matchableAmountS = order.actual.amountS - pendingAmountS
      matchableAmountS > 0
    }

    orders.collectFirst {
      case order: Order[T] if filterMethod(order) ⇒
        val original = order.original
        val r = Rational(matchableAmountS, original.amountS)

        val updatedOrder = order.copy(matchable_ = Some(OrderState(
          matchableAmountS,
          (r * Rational(original.amountB)).bigintValue,
          (r * Rational(original.amountFee)).bigintValue
        )))

        log.debug(s"top maker order: $updatedOrder")
        updatedOrder
    }
  }
}
