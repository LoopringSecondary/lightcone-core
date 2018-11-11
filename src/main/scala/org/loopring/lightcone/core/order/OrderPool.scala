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

package org.loopring.lightcone.core.order
import org.loopring.lightcone.core.data._

import org.slf4s.Logging

class OrderPool extends Object with Logging {

  type Callback = Order ⇒ Unit

  private var callbacks = Seq.empty[Callback]
  private var orderMap = Map.empty[String, Order]

  def apply(id: String): Order = orderMap(id)
  def getOrder(id: String): Option[Order] = orderMap.get(id)
  def contains(id: String): Boolean = orderMap.contains(id)
  def orders() = orderMap.values
  def add(id: String, order: Order): Unit = orderMap += id -> order
  def del(id: String): Unit = orderMap -= id
  def size: Int = orderMap.size

  private[core] def toMap = orderMap

  def addCallback(callback: Callback) = {
    callbacks :+= callback
    callbacks
  }

  def removeCallback(callback: Callback) = {
    callbacks = callbacks.dropWhile(_ == callback)
    callbacks
  }

  def callback(order: Order): Unit = callbacks.foreach(_(order))

  def +=(order: Order) = {
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
