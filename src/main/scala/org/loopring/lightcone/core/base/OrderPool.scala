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

class OrderPool extends Object with Logging {

  type Callback = Order ⇒ Unit

  private var callbacks = Seq.empty[Callback]
  private var orderMap = Map.empty[ID, Order]

  def apply(id: ID): Order = orderMap(id)
  def getOrder(id: ID): Option[Order] = orderMap.get(id)
  def contains(id: ID): Boolean = orderMap.contains(id)
  def orders() = orderMap.values

  def addCallback(callback: Callback) = {
    callbacks :+= callback
    callbacks
  }

  def removeCallback(callback: Callback) = {
    callbacks = callbacks.dropWhile(_ == callback)
    callbacks
  }

  private[core] def +=(order: Order) = {
    orderMap.get(order.id) match {
      case Some(existing) if existing == order ⇒

      case _ ⇒ order.status match {
        case OrderStatus.NEW ⇒
          orderMap += order.id -> order

        case OrderStatus.PENDING ⇒
          orderMap += order.id -> order
          callbacks.foreach(_(order))

        case _ ⇒
          log.debug("drop_order_from_pool: " + order)
          orderMap -= order.id
          callbacks.foreach(_(order))
      }
    }
  }
}
