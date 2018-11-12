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

  class View(level: Int) {
    private[depth] val sellSide = new OrderbookViewSide.Buys(level, config)
    private[depth] val buySide = new OrderbookViewSide.Sells(level, config)

    def handleUpdate(update: OrderbookUpdate) = {
      sellSide.setSlots(update.sells)
      buySide.setSlots(update.buys)
    }

    def getOrderbook(size: Int) = Orderbook(
      sellSide.getDepth(size),
      buySide.getDepth(size)
    )
  }
}
