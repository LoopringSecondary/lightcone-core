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

trait OrderManager {
  def hasTokenManager(token: Address): Boolean
  def addTokenManager(tm: TokenManager): TokenManager
  def getTokenManager(token: Address): TokenManager

  def submitOrder(order: Order): Boolean
  def cancelOrder(orderId: ID): Boolean
  def adjustOrder(orderId: ID, outstandingAmountS: Amount): Boolean
}

object OrderManager {
  def default(
    maxNumOrders: Int = 1000
  )(
    implicit
    orderPool: OrderPool
  ): OrderManager =
    new OrderManagerImpl(maxNumOrders)
}
