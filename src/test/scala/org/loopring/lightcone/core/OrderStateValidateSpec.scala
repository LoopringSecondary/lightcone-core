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

class OrderStateValidateSpec extends FlatSpec with Matchers {

  case class Raw()

  type MyOrder = Order[Raw]
  type MyTokenManager = TokenManager[Raw]
  type MyOrderPool = OrderPool[Raw]

  implicit val orderPool = new MyOrderPool()

  var receivedOrders = Map.empty[String, MyOrder]

  orderPool.addCallback(
    (order: MyOrder) ⇒ {
      receivedOrders += order.id -> order
    }
  )

  val manager = OrderStateManager.default[Raw]
  val lrc = "LRC"
  val xyz = "XYZ"
  val gto = "GTO"

  manager.addTokenManager(new MyTokenManager(lrc))
  manager.addTokenManager(new MyTokenManager(xyz))
  manager.addTokenManager(new MyTokenManager(gto))

  val lrcTokenManager = manager.getTokenManager(lrc)
  val xyzTokenManager = manager.getTokenManager(xyz)
  val gtoTokenManager = manager.getTokenManager(gto)

  // tokenManager订单size测试
  "testTokenSize" should "add new TokenManager" in {
    info("[sbt core/'testOnly *OrderStateValidateSpec -- -z testTokenSize']")

    lrcTokenManager.reset(100, 200)

    val order = Order(
      Raw(),
      "order1",
      lrc,
      xyz,
      None,
      10,
      2,
      0
    )
    manager.submitOrder(order)

    receivedOrders.getOrElse("order1", order).status should be(OrderStatus.PENDING)
  }

  // 测试账户余额不足情况下下单后被取消
  "testInSufficientFunds" should "cancel order" in {
    info("[sbt core/'testOnly *OrderStateValidateSpec -- -z testInSufficientFunds']")

    lrcTokenManager.reset(100, 200)

    manager.submitOrder(Order(
      Raw(),
      "order1",
      lrc,
      xyz,
      None,
      80,
      2,
      0
    )) should be(true)

    manager.submitOrder(Order(
      Raw(),
      "order2",
      lrc,
      xyz,
      None,
      18,
      2,
      6
    )) should be(true)

    manager.submitOrder(Order(
      Raw(),
      "order3",
      lrc,
      xyz,
      None,
      80,
      2,
      0
    )) should be(false)

    manager.submitOrder(Order(
      Raw(),
      "order4",
      lrc,
      xyz,
      Option(xyz),
      80,
      20,
      10
    )) should be(true)
  }
}
