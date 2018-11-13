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

  def processUpdate(update: OrderbookUpdate) = {
    val diff = viewMap(0).getDiff(update)
    viewMap.values.foreach(_.processUpdate(diff))
  }

  def getOrderbook(level: Int, size: Int) = viewMap.get(level) match {
    case Some(view) ⇒ view.getOrderbook(size)
    case None       ⇒ Orderbook(Nil, Nil)
  }

  def reset() = viewMap.values.foreach(_.reset)

  private[depth] class View(aggregationLevel: Int) {

    private val priceFormat = s"%.${config.precisionForPrice}f"
    private val amountFormat = s"%.${config.precisionForAmount}f"
    private val totalFormat = s"%.${config.precisionForTotal}f"

    private val sellSide = new OrderbookSide.Sells(
      config.priceDecimals, aggregationLevel, false
    ) with ConverstionSupport

    private val buySide = new OrderbookSide.Buys(
      config.priceDecimals, aggregationLevel, false
    ) with ConverstionSupport

    def processUpdate(update: OrderbookUpdate) {
      update.sells.foreach(sellSide.increase)
      update.buys.foreach(buySide.increase)
    }

    def getDiff(update: OrderbookUpdate) = {
      OrderbookUpdate(
        update.sells.map(sellSide.getDiff),
        update.buys.map(buySide.getDiff)
      )
    }

    def getOrderbook(size: Int) =
      Orderbook(sellSide.getDepth(size), buySide.getDepth(size))

    def reset() {
      sellSide.reset()
      buySide.reset()
    }

    trait ConverstionSupport { self: OrderbookSide ⇒
      private def slotToItem(slot: OrderbookSlot) = OrderbookItem(
        priceFormat.format(slot.slot / priceScaling),
        amountFormat.format(slot.amount),
        totalFormat.format(slot.total)
      )
      def getDepth(num: Int): Seq[OrderbookItem] = getSlots(num).map(slotToItem(_))
    }
  }
}

