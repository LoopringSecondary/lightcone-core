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

package org.loopring.lightcone.core.depth

import org.loopring.lightcone.core.data._
import scala.collection.SortedMap

class OrderbookAggregator(marketId: MarketId, priceDecimals: Int) {
  private val sells = new OrderbookAggregatorSide.Sells(priceDecimals)
  private val buys = new OrderbookAggregatorSide.Buys(priceDecimals)

  def getOrderbookUpdate(num: Int = 0): OrderbookUpdate = {
    if (num == 0) OrderbookUpdate(buys.takeUpdatedSlots, buys.takeUpdatedSlots)
    else OrderbookUpdate(buys.getSlots(num), buys.getSlots(num))
  }

  def adjustAmount(
    isSell: Boolean,
    increase: Boolean,
    price: Double,
    amount: Double,
    total: Double
  ) {
    val side = if (isSell) sells else buys
    side.adjustAmount(increase, price, amount, total)
  }
}

private object OrderbookAggregatorSide {
  class Sells(val priceDecimals: Int)
    extends LongOrderingSupport(true) with OrderbookAggregatorSide

  class Buys(val priceDecimals: Int)
    extends LongOrderingSupport(false) with OrderbookAggregatorSide
}

private trait OrderbookAggregatorSide {
  val isSell: Boolean
  val priceDecimals: Int
  implicit val ordering: Ordering[Long]

  private val scaling = Math.pow(10, priceDecimals)
  private var slotMap = SortedMap.empty[Long, OrderbookSlot]
  private var updatedSlots = Map.empty[Long, OrderbookSlot]

  def adjustAmount(
    increase: Boolean,
    price: Double,
    amount: Double,
    total: Double
  ) = {
    val id = getSlotForPriceId(price)
    val op =
      if (increase) (a: Double, b: Double) ⇒ Math.max(0, a + b)
      else (a: Double, b: Double) ⇒ Math.max(0, a - b)

    val slot = slotMap.get(id) match {
      case Some(slot) ⇒ OrderbookSlot(id, op(slot.amount, amount), op(slot.total, total))
      case None       ⇒ OrderbookSlot(id, op(0, amount), op(0, total))
    }

    slotMap += id -> slot
    updatedSlots += id -> slot
  }

  def getSlots(num: Int): Seq[OrderbookSlot] = slotMap.take(num).values.toSeq

  def takeUpdatedSlots(): Seq[OrderbookSlot] = {
    val slots = updatedSlots.values.toSeq
    updatedSlots = Map.empty
    slots
  }

  private def getSlotForPriceId(price: Double) = {
    if (isSell) Math.ceil(price * scaling).toLong
    else Math.floor(price * scaling).toLong
  }
}

