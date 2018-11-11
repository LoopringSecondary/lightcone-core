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
import org.loopring.lightcone.core._
import org.scalatest._

class TokenReserveManager_AccumulatedAmountSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *AccumulatedAmountSpec']")

  // 简单的测试1: 下两个订单,总金额不超过账户总金额
  // reservation应该是两个订单balance/allowance之和
  "simpleTest1" should "with the same amount" in {
    val (manager, _, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager) = prepare

    lrcTokenReserveManager.init(100, 100)
    xyzTokenReserveManager.init(1000, 1000)
    gtoTokenReserveManager.init(1000, 1000)

    manager.submitOrder(Order(
      "order1",
      lrc,
      xyz,
      lrc,
      20,
      10,
      10
    ))

    manager.submitOrder(Order(
      "order2",
      lrc,
      xyz,
      lrc,
      30,
      10,
      20
    ))

    lrcTokenReserveManager.reservations.head.accumulatedBalance should be(30)
    lrcTokenReserveManager.reservations.last.accumulatedBalance should be(80)
  }

  // 简单的测试2: 下两个订单后重置账户,账户总金额不够
  // reservations 应该是一个订单
  "simpleTest2" should "validate reservation size and item balance/allowance" in {
    val (manager, _, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager) = prepare

    lrcTokenReserveManager.init(100, 100)
    xyzTokenReserveManager.init(1000, 1000)
    gtoTokenReserveManager.init(1000, 1000)

    manager.submitOrder(Order(
      "order1",
      lrc,
      xyz,
      lrc,
      20,
      10,
      10
    ))

    manager.submitOrder(Order(
      "order2",
      lrc,
      xyz,
      lrc,
      30,
      10,
      20
    ))

    lrcTokenReserveManager.init(40, 100)
    lrcTokenReserveManager.reservations.size should be(1)
    lrcTokenReserveManager.reservations.head.accumulatedBalance should be(30)
    lrcTokenReserveManager.reservations.head.accumulatedAllowance should be(30)
  }

  // tokenManager被设计为不允许balance不足,但是允许allowance不足
  // 简单的测试3: 下两个订单后重置账户,账户总授权不够
  // reservations 应该是一个订单
  "simpleTest3" should "validate reservation size and item balance/allowance" in {
    val (manager, _, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager) = prepare

    lrcTokenReserveManager.init(100, 100)
    xyzTokenReserveManager.init(1000, 1000)
    gtoTokenReserveManager.init(1000, 1000)

    manager.submitOrder(Order(
      "order1",
      lrc,
      xyz,
      lrc,
      20,
      10,
      10
    ))

    manager.submitOrder(Order(
      "order2",
      lrc,
      xyz,
      lrc,
      30,
      10,
      20
    ))

    lrcTokenReserveManager.init(100, 40)
    lrcTokenReserveManager.reservations.size should be(2)
    lrcTokenReserveManager.reservations.head.accumulatedBalance should be(30)
    lrcTokenReserveManager.reservations.head.accumulatedAllowance should be(30)

    lrcTokenReserveManager.reservations.last.accumulatedBalance should be(80)
    lrcTokenReserveManager.reservations.last.accumulatedAllowance should be(40)
  }
}
