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

import helper._
import org.scalatest._

class RequestAmountSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *RequestAmountSpec']")

  // 多种情况测试orderRequest
  // 注意: tokenManager在判断requestedAmount时,允许等于
  "testRequestAmount" should "cancel order" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(100, 200)
    xyzTokenManager.init(100, 200)
    gtoTokenManager.init(100, 200)

    // 情况1:tokenFee only, balance/allowance充足
    manager.submitOrder(Order(
      "order",
      lrc,
      xyz,
      gto,
      50,
      20,
      50
    )) should be(true)
    manager.cancelOrder("order")

    // 情况2: tokenFee only, balance/allowance不足
    manager.submitOrder(Order(
      "order",
      lrc,
      xyz,
      gto,
      50,
      20,
      110
    )) should be(false)

    // 情况3: tokenFee == tokenS, balance/allowance充足
    manager.submitOrder(Order(
      "order",
      lrc,
      xyz,
      lrc,
      30,
      10,
      10
    )) should be(true)
    manager.cancelOrder("order")

    // 情况4: tokenFee == tokenS, balance/allowance不足
    manager.submitOrder(Order(
      "order",
      lrc,
      xyz,
      lrc,
      100,
      10,
      10
    )) should be(false)

    // 情况5: tokenFee == tokenB, balance/allowance充足
    manager.submitOrder(Order(
      "order",
      lrc,
      xyz,
      xyz,
      30,
      20,
      120
    )) should be(true)
    manager.cancelOrder("order")

    // 情况6: tokenFee == tokenB, balance/allowance不足
    manager.submitOrder(Order(
      "order",
      lrc,
      xyz,
      xyz,
      30,
      20,
      200
    )) should be(false)
  }
}
