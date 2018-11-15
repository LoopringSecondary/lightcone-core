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

private[depth] object OrderbookSide {
  class Sells(
      val priceDecimals: Int,
      val aggregationLevel: Int,
      val maintainUpdatedSlots: Boolean
  ) extends LongOrderingSupport(true) with OrderbookSide

  class Buys(
      val priceDecimals: Int,
      val aggregationLevel: Int,
      val maintainUpdatedSlots: Boolean
  ) extends LongOrderingSupport(false) with OrderbookSide
}

private[depth] trait OrderbookSide {
  val isSell: Boolean
  val maintainUpdatedSlots: Boolean
  val priceDecimals: Int
  val aggregationLevel: Int
  implicit val ordering: Ordering[Long]

  val aggregationScaling = Math.pow(10, aggregationLevel)
  val priceScaling = Math.pow(10, priceDecimals)
  var slotMap = SortedMap.empty[Long, OrderbookSlot]

  var oldSlots = Map.empty[Long, OrderbookSlot]
  var updatedSlots = Map.empty[Long, OrderbookSlot]

  def increase(price: Double, amount: Double, total: Double): Unit =
    increase(OrderbookSlot(getSlotForPriceId(price), amount, total))

  def decrease(price: Double, amount: Double, total: Double): Unit =
    decrease(OrderbookSlot(getSlotForPriceId(price), amount, total))

  def replace(price: Double, amount: Double, total: Double): Unit =
    replace(OrderbookSlot(getSlotForPriceId(price), amount, total))

  def increase(slot: OrderbookSlot): Unit = adjustInternal(slot, _ + _)

  def decrease(slot: OrderbookSlot): Unit = adjustInternal(slot, _ - _)

  def replace(slot: OrderbookSlot): Unit =
    adjustInternal(slot, (old: OrderbookSlot, new_ : OrderbookSlot) ⇒ new_)

  def getDiff(slot: OrderbookSlot) = {
    slot - slotMap.getOrElse(slot.slot, OrderbookSlot(slot.slot, 0, 0))
  }

  private def adjustInternal(
    slot: OrderbookSlot,
    op: (OrderbookSlot, OrderbookSlot) ⇒ OrderbookSlot
  ) = {
    val id = getAggregationSlotFor(slot.slot)

    val old = slotMap.getOrElse(id, OrderbookSlot(id, 0, 0))
    if (maintainUpdatedSlots && !oldSlots.contains(id)) {
      oldSlots += id -> old
    }

    var updated = op(old, slot.copy(slot = id))
    if (updated.amount <= 0 || updated.total <= 0) {
      updated = OrderbookSlot(id, 0, 0)
      slotMap -= id
    } else {
      slotMap += id -> updated
    }
    if (maintainUpdatedSlots && old != updated) {
      updatedSlots += id -> updated
    }
  }

  def reset() = {
    slotMap = SortedMap.empty
    oldSlots = Map.empty
    updatedSlots = Map.empty
  }
  def getSlots(num: Int): Seq[OrderbookSlot] =
    slotMap.take(num).values.toList

  def takeUpdatedSlots(): Seq[OrderbookSlot] = {
    if (!maintainUpdatedSlots) {
      throw new UnsupportedOperationException("maintainUpdatedSlots is false")
    }
    val slots = updatedSlots.filter {
      case (id, slot) ⇒ oldSlots(id) != slot
    }.values.toList

    oldSlots = Map.empty
    updatedSlots = Map.empty
    slots
  }

  private[depth] def getAggregationSlotFor(slot: Long) = {
    if (isSell) (Math.ceil(slot / aggregationScaling) * aggregationScaling).toLong
    else (Math.floor(slot / aggregationScaling) * aggregationScaling).toLong
  }

  private[depth] def getSlotForPriceId(price: Double) = {
    if (isSell) Math.ceil(price * priceScaling).toLong
    else Math.floor(price * priceScaling).toLong
  }
}

