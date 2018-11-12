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

private[depth] trait OrderbookViewSide {
  val level: Int
  val config: OrderbookConfig
  val isSell: Boolean
  implicit val ordering: Ordering[Long]

  private val scaling = Math.pow(10, config.priceDecimals - level)
  private val priceFormat = s"%.${config.precisionForPrice}f"
  private val amountFormat = s"%.${config.precisionForAmount}f"
  private val totalFormat = s"%.${config.precisionForTotal}f"

  var slotMap = SortedMap.empty[Long, OrderbookItem]

  def setSlots(slots: Seq[OrderbookSlot]) = slots.foreach(setSlot)

  def setSlot(slot: OrderbookSlot) = {
    slotMap += slot.slot -> OrderbookItem(
      priceFormat.format(slot.slot / scaling),
      amountFormat.format(slot.amount),
      totalFormat.format(slot.total)
    )
  }

  def getDepth(num: Int): Seq[OrderbookItem] = slotMap.take(num).values.toSeq
}

private[depth] object OrderbookViewSide {
  class Sells(val level: Int, val config: OrderbookConfig)
    extends OrderbookViewSide {
    val isSell = true

    implicit val ordering = new Ordering[Long] {
      def compare(a: Long, b: Long) = a compare b
    }
  }

  class Buys(val level: Int, val config: OrderbookConfig)
    extends OrderbookViewSide {
    val isSell = false

    implicit val ordering = new Ordering[Long] {
      def compare(a: Long, b: Long) = b compare a
    }
  }
}
