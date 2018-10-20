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

class RequestAmountSpec extends FlatSpec with Matchers {

  import Helper._

  info("[sbt core/'testOnly *RequestAmountSpec']")

  implicit val orderPool = new MyOrderPool()

  var receivedOrders = Map.empty[String, MyOrder]

  orderPool.addCallback(
    (order: MyOrder) ⇒ {
      receivedOrders += order.id -> order
    }
  )

  val manager = OrderStateManager.default[Raw]()
  val lrc = "LRC"
  val xyz = "XYZ"
  val gto = "GTO"

  manager.addTokenManager(new MyTokenManager(lrc))
  manager.addTokenManager(new MyTokenManager(xyz))
  manager.addTokenManager(new MyTokenManager(gto))

  val lrcTokenManager = manager.getTokenManager(lrc)
  val xyzTokenManager = manager.getTokenManager(xyz)
  val gtoTokenManager = manager.getTokenManager(gto)

  // 多种情况测试orderRequest
  // 注意: tokenManager在判断requestedAmount时,允许等于
  "testRequestAmount" should "cancel order" in {
    lrcTokenManager.init(100, 200)
    xyzTokenManager.init(100, 200)
    gtoTokenManager.init(100, 200)

    // 情况1:tokenFee == None, balance/allowance充足
    manager.submitOrder(newOrder(
      "order",
      lrc,
      xyz,
      None,
      50,
      20,
      50
    )) should be(true)
    manager.cancelOrder("order")

    // 情况2: tokenFee == None, balance/allowance不足
    manager.submitOrder(newOrder(
      "order",
      lrc,
      xyz,
      None,
      50,
      20,
      60
    )) should be(false)

    // 情况3: tokenFee == tokenS, balance/allowance充足
    manager.submitOrder(newOrder(
      "order",
      lrc,
      xyz,
      Option(lrc),
      30,
      10,
      10
    )) should be(true)
    manager.cancelOrder("order")

    // 情况4: tokenFee == tokenS, balance/allowance不足
    manager.submitOrder(newOrder(
      "order",
      lrc,
      xyz,
      Option(lrc),
      100,
      10,
      10
    )) should be(false)

    // 情况5: tokenFee == tokenB, balance/allowance充足
    manager.submitOrder(newOrder(
      "order",
      lrc,
      xyz,
      Option(xyz),
      30,
      20,
      120
    )) should be(true)
    manager.cancelOrder("order")

    // 情况6: tokenFee == tokenB, balance/allowance不足
    manager.submitOrder(newOrder(
      "order",
      lrc,
      xyz,
      Option(xyz),
      30,
      20,
      200
    )) should be(false)
  }
}
