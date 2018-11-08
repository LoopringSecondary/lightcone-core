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
import helper._

class ActualAmountSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *ActualAmountSpec']")

  // 情况1:tokens!=tokenb!=tokenfee 账户充足
  "simpleTest1" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 1000)
    xyzTokenManager.init(1000, 1000)
    gtoTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      xyz,
      lrc,
      gto,
      100,
      100,
      20
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(100)
    state.actual.amountB should be(100)
    state.actual.amountFee should be(20)
  }

  // 情况2:tokens!=tokenb!=tokenfee tokens不充足
  "simpleTest2" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    xyzTokenManager.init(1000, 10)
    lrcTokenManager.init(1000, 1000)
    gtoTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      xyz,
      lrc,
      gto,
      100,
      100,
      20
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(10)
    state.actual.amountB should be(10)
    state.actual.amountFee should be(2)
  }

  // 情况3:tokens==tokenfee tokens充足
  "simpleTest3" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 1000)
    xyzTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      lrc,
      xyz,
      lrc,
      100,
      100,
      20
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(100)
    state.actual.amountB should be(100)
    state.actual.amountFee should be(20)
  }

  // 情况4:tokens==tokenfee tokens不足
  "simpleTest4" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 60)
    xyzTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      lrc,
      xyz,
      lrc,
      100,
      100,
      20
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(50)
    state.actual.amountB should be(50)
    state.actual.amountFee should be(10)
  }

  // 情况5: tokenb == tokenfee, tokenb > tokenfee 余额充足
  "simpleTest5" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 1000)
    xyzTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      xyz,
      lrc,
      lrc,
      100,
      100,
      20
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(100)
    state.actual.amountB should be(100)
    state.actual.amountFee should be(20)
  }

  // 情况6: tokenb == tokenfee, tokenb > tokenfee 余额不足
  "simpleTest6" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 10)
    xyzTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      xyz,
      lrc,
      lrc,
      100,
      100,
      20
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(100)
    state.actual.amountB should be(100)
    state.actual.amountFee should be(20)
  }

  // 情况7: tokenb == tokenfee, tokenb < tokenfee 余额充足
  "simpleTest7" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 1000)
    xyzTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      xyz,
      lrc,
      lrc,
      100,
      100,
      200
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(100)
    state.actual.amountB should be(100)
    state.actual.amountFee should be(200)
  }

  // 情况8: tokenb == tokenfee, tokenb < tokenfee 余额充足
  "simpleTest8" should "calculate actual amount" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 50)
    xyzTokenManager.init(1000, 1000)

    val order = Order(
      "order",
      xyz,
      lrc,
      lrc,
      100,
      100,
      200
    )
    manager.submitOrder(order)

    val state = orderPool("order")

    state.actual.amountS should be(50)
    state.actual.amountB should be(50)
    state.actual.amountFee should be(100)
  }
}
