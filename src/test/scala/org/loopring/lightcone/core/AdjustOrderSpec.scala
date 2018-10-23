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

import org.scalatest._

class AdjustOrderSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *AdjustOrderSpec']")

  implicit val orderPool = new OrderPool()

  var receivedOrders = Map.empty[String, Order]

  orderPool.addCallback(
    (order: Order) ⇒ {
      receivedOrders += order.id -> order
    }
  )

  val manager = OrderStateManager.default()
  val lrc = "LRC"
  val xyz = "XYZ"
  val gto = "GTO"

  manager.addTokenManager(new TokenManager(lrc))
  manager.addTokenManager(new TokenManager(xyz))
  manager.addTokenManager(new TokenManager(gto))

  val lrcTokenManager = manager.getTokenManager(lrc)
  val xyzTokenManager = manager.getTokenManager(xyz)
  val gtoTokenManager = manager.getTokenManager(gto)

  lrcTokenManager.init(200, 200)
  xyzTokenManager.init(200, 200)
  gtoTokenManager.init(200, 200)

  "simpleTest1" should "submit order and fill partitial" in {
    val order = Order(
      "order",
      lrc,
      xyz,
      gto,
      100,
      100,
      100
    )

    manager.submitOrder(order)
    manager.adjustOrder(order.id, 60)

    val state = orderPool(order.id)
    state.requestedAmount()(lrc) should be(60)
    state.reservedAmount()(lrc) should be(60)
  }
}
