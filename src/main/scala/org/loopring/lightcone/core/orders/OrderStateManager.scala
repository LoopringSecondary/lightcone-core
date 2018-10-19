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

trait OrderStateManager[T] {
  def hasTokenManager(token: Address): Boolean
  def addTokenManager(tm: TokenManager[T]): TokenManager[T]
  def getTokenManager(token: Address): TokenManager[T]

  def submitOrder(order: Order[T]): Boolean
  def cancelOrder(orderId: ID): Boolean
  def adjustOrder(orderId: ID, amountSDelta: Amount): Boolean
}

object OrderStateManager {
  def default[T](maxNumOrders: Int = 1000)(implicit orderPool: OrderPool[T]): OrderStateManager[T] =
    new OrderStateManagerImpl[T](maxNumOrders)(orderPool)
}
