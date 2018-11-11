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

trait OrderManager {
  def hasTokenReserveManager(token: String): Boolean
  def addTokenReserveManager(tm: TokenReserveManager): TokenReserveManager
  def getTokenReserveManager(token: String): TokenReserveManager

  def submitOrder(order: Order): Boolean
  def cancelOrder(orderId: String): Boolean
  def adjustOrder(orderId: String, outstandingAmountS: BigInt): Boolean
}

object OrderManager {
  def default()(implicit orderPool: OrderPool): OrderManager = new OrderManagerImpl()
}
