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

class DepthOrderPoolImpl extends OrderPool[DepthOrder] with Logging {

  // amount为订单剩余量
  def +=(order: DepthOrder): Unit = {
    getOrder(order.id) match {
      case Some(existing) ⇒
        if (order.amountS > 0) {
          add(order.id, order)
        } else {
          log.debug("drop_order_from_pool: " + order.id)
          del(order.id)
        }
        callback(order)

      case _ ⇒
        if (order.amountS > 0) {
          add(order.id, order)
          callback(order)
        } else {
          log.debug("order not exist:" + order.id + "while del")
        }
    }
  }
}
