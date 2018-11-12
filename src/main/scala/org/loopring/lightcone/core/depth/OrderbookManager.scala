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

class OrderbookManager(config: OrderbookConfig) {
  private[depth] val viewMap = (0 until config.levels).map {
    level ⇒ level -> new View(level)
  }.toMap

  def handleUpdate(update: OrderbookUpdate) = {
    viewMap.values.foreach(_.handleUpdate(update))
  }

  def getOrderbook(level: Int, size: Int) = viewMap.get(level) match {
    case Some(view) ⇒ view.getOrderbook(size)
    case None       ⇒ Orderbook(Nil, Nil)
  }

  def reset() = viewMap.values.foreach(_.reset)

  class View(level: Int) {
    private val sellSide = new OrderbookViewSide.Sells(level, config)
    private val buySide = new OrderbookViewSide.Buys(level, config)

    def handleUpdate(update: OrderbookUpdate) = {
      sellSide.setSlots(update.sells)
      buySide.setSlots(update.buys)
    }

    def getOrderbook(size: Int) = Orderbook(
      sellSide.getDepth(size),
      buySide.getDepth(size)
    )

    def reset() = {
      sellSide.reset()
      buySide.reset()
    }
  }
}

private object OrderbookViewSide {
  class Sells(val level: Int, val config: OrderbookConfig)
    extends LongOrderingSupport(true) with OrderbookViewSide

  class Buys(val level: Int, val config: OrderbookConfig)
    extends LongOrderingSupport(false) with OrderbookViewSide
}

private trait OrderbookViewSide {
  val level: Int
  val config: OrderbookConfig
  val isSell: Boolean
  implicit val ordering: Ordering[Long]

  private val scaling = Math.pow(10, config.priceDecimals - level)
  private val priceFormat = s"%.${config.precisionForPrice}f"
  private val amountFormat = s"%.${config.precisionForAmount}f"
  private val totalFormat = s"%.${config.precisionForTotal}f"

  var itemMap = SortedMap.empty[Long, OrderbookItem]

  def setSlots(slots: Seq[OrderbookSlot]) = slots.foreach(setSlot)

  def setSlot(slot: OrderbookSlot) = {
    itemMap += slot.slot -> OrderbookItem(
      priceFormat.format(slot.slot / scaling),
      amountFormat.format(slot.amount),
      totalFormat.format(slot.total)
    )
  }

  def reset() = { itemMap = SortedMap.empty }

  def getDepth(num: Int): Seq[OrderbookItem] = itemMap.take(num).values.toList
}
