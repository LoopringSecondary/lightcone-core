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

case class MarketId(
    primary: Address,
    secondary: Address
)

case class OrderBookConfig(
    maxNumBuys: Int,
    maxNumSells: Int,
    maxNumHiddenBuys: Int,
    maxNumHiddenSells: Int
)

abstract class OrderBookImpl[T](
    marketId: MarketId,
    config: OrderBookConfig
)(
    implicit
    pendingRingPool: PendingRingPool[T],
    orderPool: OrderPool[T]
) extends OrderBook[T] with Logging {

  private val sides = Map(
    marketId.primary -> new OrderBookSide[T](marketId.primary),
    marketId.secondary -> new OrderBookSide[T](marketId.secondary)
  )

  private var lastPrice: Option[Rational] = None

  def addOrder(order: Order[T]): Set[Ring[T]] = {
    order.matchable
    null
  }
  def deleteOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]] = {
    null
  }

  def getLastPrice(): Option[Rational] = lastPrice
  def getMetadata(): OrderBookMetadata

  def getTops(isPrimary: Boolean, num: Int, skip: Int = 0, includingHidden: Boolean = false) = {
    val side = if (isPrimary) sides(marketId.primary) else sides(marketId.secondary)
    side.getTops(num, skip, includingHidden)
  }

  // Implicit class
  implicit private class RichOrderInMarket[T](order: Order[T]) {
    def matchable() = {
      val pendingAmountS = pendingRingPool.getOrderPendingAmountS(order.id)
      if (pendingAmountS == 0) order.actual
      else {
        val actual = order.actual
        val original = order.original
        assert(actual.amountS > 0)
        val amountS = (actual.amountS - pendingAmountS).max(0)
        val r = Rational(amountS, original.amountS)

        Amounts(
          amountS,
          (r * Rational(original.amountB)).bigintValue,
          (r * Rational(original.amountFee)).bigintValue,
        )
      }

    }
  }
}
