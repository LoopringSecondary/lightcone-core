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
  private val sellSide = new OrderbookAggregatorSellSide(priceDecimals)
  private val buySide = new OrderbookAggregatorBuySide(priceDecimals)

  def getSlots(num: Int = 0): OrderbookUpdate =
    if (num == 0) OrderbookUpdate(sellSide.takeUpdatedSlots, buySide.takeUpdatedSlots)
    else OrderbookUpdate(sellSide.getSlots(num), buySide.getSlots(num))
}

private trait OrderbookAggregatorSide {
  val isSell: Boolean
  val priceDecimals: Int
  implicit val ordering: Ordering[Long]

  private lazy val scaling = Math.pow(10, priceDecimals)

  var slotMap = SortedMap.empty[Long, OrderbookSlot]
  var updatedSlots = Map.empty[Long, OrderbookSlot]

  def increase(price: Double, amount: Double, total: Double) =
    updateSlot(price, amount, total)(_ + _)

  def decrease(price: Double, amount: Double, total: Double) =
    updateSlot(price, amount, total)(_ - _)

  private def updateSlot(
    price: Double,
    amount: Double,
    total: Double
  )(op: (Double, Double) ⇒ Double) = {
    val id = getSlotForPriceId(price)
    val op_ = (a: Double, b: Double) ⇒ Math.max(0, op(a, b))

    val slot = slotMap.get(id) match {
      case Some(slot) ⇒ OrderbookSlot(id, op_(slot.amount, amount), op_(slot.total, total))
      case None       ⇒ OrderbookSlot(id, op_(0, amount), op(0, total))
    }

    slotMap += id -> slot
    updatedSlots += id -> slot
  }

  private def getSlotForPriceId(price: Double) = {
    if (isSell) Math.ceil(price * scaling).toLong
    else Math.floor(price * scaling).toLong
  }

  def getSlots(num: Int): Seq[OrderbookSlot] = slotMap.take(num).values.toSeq

  def takeUpdatedSlots(): Seq[OrderbookSlot] = {
    val slots = updatedSlots.values.toSeq
    updatedSlots = Map.empty
    slots
  }
}

private class OrderbookAggregatorSellSide(val priceDecimals: Int)
  extends OrderbookAggregatorSide {
  val isSell = true

  implicit val ordering = new Ordering[Long] {
    def compare(a: Long, b: Long) = a compare b
  }
}

private class OrderbookAggregatorBuySide(val priceDecimals: Int)
  extends OrderbookAggregatorSide {
  val isSell = false

  implicit val ordering = new Ordering[Long] {
    def compare(a: Long, b: Long) = b compare a
  }
}

