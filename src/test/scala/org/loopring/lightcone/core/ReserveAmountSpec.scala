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

class ReserveAmountSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *ReserveAmountSpec']")

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

  lrcTokenManager.init(1000, 1000)
  xyzTokenManager.init(1000, 1000)
  gtoTokenManager.init(1000, 1000)

  // 情况1:tokenFee only, allowance充足
  "simpleTest1" should "calculate reserve amount" in {
    val order = Order(
      "order",
      gto,
      xyz,
      lrc,
      100,
      20,
      100
    )
    manager.submitOrder(order)

    lrcTokenManager.init(1000, 200)
    val state = orderPool("order")

    state.reservedAmount()(lrc) should be(100)
  }

  // 情况2: tokenFee only, allowance不足
  "simpleTest2" should "calculate reserve amount" in {
    val order = Order(
      "order",
      gto,
      xyz,
      lrc,
      100,
      20,
      100
    )
    manager.submitOrder(order)

    lrcTokenManager.init(1000, 50)
    val state = orderPool("order")

    state.reservedAmount()(lrc) should be(50)
  }

  // 情况3: tokenFee == tokenS, allowance充足
  "simpleTest3" should "calculate reserve amount" in {
    val order = Order(
      "order",
      lrc,
      xyz,
      lrc,
      100,
      20,
      50
    )
    manager.submitOrder(order)

    lrcTokenManager.init(1000, 200)
    val state = orderPool("order")

    state.reserved.amountS should be(100)
    state.reserved.amountFee should be(50)
    state.reservedAmount()(lrc) should be(150)
  }

  // 情况4: tokenFee == tokenS, allowance不足
  "simpleTest4" should "calculate reserve amount" in {
    val order = Order(
      "order",
      lrc,
      xyz,
      lrc,
      100,
      20,
      200
    )
    manager.submitOrder(order)

    lrcTokenManager.init(1000, 200)
    val state = orderPool("order")

    state.reserved.amountS.intValue() should be(66)
    state.reserved.amountFee.intValue() should be(134)
    state.reservedAmount()(lrc).intValue() should be(200)
  }

  // 情况5: tokenFee == tokenB, allowance充足
  "simpleTest5" should "calculate reserve amount" in {
    val order = Order(
      "order",
      lrc,
      xyz,
      xyz,
      100,
      20,
      200
    )
    manager.submitOrder(order)

    xyzTokenManager.init(1000, 200)
    orderPool("order").reservedAmount()(xyz) should be(180)
  }

  // 情况6: tokenFee == tokenB, allowance不足
  "simpleTest6" should "calculate reserve amount" in {
    val order = Order(
      "order",
      lrc,
      xyz,
      xyz,
      100,
      100,
      400
    )
    manager.submitOrder(order)

    xyzTokenManager.init(1000, 200)
    orderPool("order").reservedAmount()(xyz) should be(200)
  }
}
