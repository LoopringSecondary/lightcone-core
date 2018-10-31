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

class OrderPoolImpl extends OrderPool[Order] with Logging {

  def +=(order: Order): Unit = {
    getOrder(order.id) match {
      case Some(existing) if existing == order ⇒

      case _ ⇒ order.status match {
        case OrderStatus.NEW ⇒
          add(order.id, order)

        case OrderStatus.PENDING ⇒
          add(order.id, order)
          callback(order)

        case _ ⇒
          log.debug("drop_order_from_pool: " + order)
          del(order.id)
          callback(order)
      }
    }
  }
}
