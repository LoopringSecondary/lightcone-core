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

  def addOrder(order: Order[T]): Seq[Ring[T]] = {
    var rings = Seq.empty[Ring[T]]

    case class State[T](taker: Order[T], pending: OrderState = OrderState())

    def recursivelyMatchOrder(state: State[T]): State[T] = {
      recursivelyMatchOrder(state)
    }

    val State(updatedOrder, pending) = recursivelyMatchOrder(State[T](order))

    rings
  }

  def deleteOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]] = {
    null
  }

  def getLastPrice(): Option[Rational] = lastPrice
  def getMetadata(): OrderBookMetadata
}
