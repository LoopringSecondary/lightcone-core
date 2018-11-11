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

import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core._
import org.scalatest._

class TokenReserveManagerSpec_AdjustOrder extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *AdjustOrderSpec']")

  "simpleTest1" should "submit order and fill partitial" in {
    val (manager, orderPool, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager) = prepare

    lrcTokenReserveManager.init(200, 200)
    xyzTokenReserveManager.init(200, 200)
    gtoTokenReserveManager.init(200, 200)

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

  // 提交订单后完全成交,看订单是否在tokenS&tokenFee manager中是否都删除了
  "simpleTest2" should "submit order and fill partitial" in {
    val (manager, orderPool, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager) = prepare

    lrcTokenReserveManager.init(200, 200)
    xyzTokenReserveManager.init(200, 200)
    gtoTokenReserveManager.init(200, 200)

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
    manager.adjustOrder(order.id, 0)

    orderPool.contains(order.id) should be(false)

    lrcTokenReserveManager.balance should be(200)
    lrcTokenReserveManager.allowance should be(200)
    gtoTokenReserveManager.balance should be(200)
    gtoTokenReserveManager.allowance should be(200)

    lrcTokenReserveManager.availableBalance should be(200)
    lrcTokenReserveManager.availableAllowance should be(200)
    gtoTokenReserveManager.availableBalance should be(200)
    gtoTokenReserveManager.availableAllowance should be(200)

    lrcTokenReserveManager.idxMap.size should be(0)
    gtoTokenReserveManager.idxMap.size should be(0)

    lrcTokenReserveManager.cursor should be(-1)
    gtoTokenReserveManager.cursor should be(-1)

    lrcTokenReserveManager.reservations.size should be(0)
    gtoTokenReserveManager.reservations.size should be(0)
  }

  // 部分成交1:
  // 连续下三个订单,tokenS对应的balance&allowance始终充足,
  // order2成交70(剩余30)后,tokenManager的订单链位置应该不受影响
  // cursor应该仍然为2,
  // reservation数量不变,accumulatedAmount从order2递减
  // balance&allowance不变
  // availableBalance&availableAllowance对应增加order2成交量
  "complexTest1" should "submit orders then fill one of them" in {
    val (manager, orderPool, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager) = prepare

    lrcTokenReserveManager.init(200, 200)
    xyzTokenReserveManager.init(200, 200)
    gtoTokenReserveManager.init(200, 200)

    val order1 = Order(
      "order1",
      lrc,
      xyz,
      gto,
      50,
      10,
      10
    )
    manager.submitOrder(order1)

    val order2 = Order(
      "order2",
      lrc,
      xyz,
      gto,
      100,
      10,
      10
    )
    manager.submitOrder(order2)

    val order3 = Order(
      "order3",
      lrc,
      xyz,
      gto,
      50,
      10,
      10
    )
    manager.submitOrder(order3)

    // order2成交70, 剩余30
    manager.adjustOrder(order2.id, 30)

    lrcTokenReserveManager.balance should be(200)
    lrcTokenReserveManager.allowance should be(200)

    lrcTokenReserveManager.availableBalance should be(70)
    lrcTokenReserveManager.availableAllowance should be(70)

    lrcTokenReserveManager.cursor should be(2)

    lrcTokenReserveManager.idxMap.size should be(3)
    lrcTokenReserveManager.idxMap.getOrElse(order1.id, -1) should be(0)
    lrcTokenReserveManager.idxMap.getOrElse(order2.id, -1) should be(1)
    lrcTokenReserveManager.idxMap.getOrElse(order3.id, -1) should be(2)

    lrcTokenReserveManager.reservations.size should be(3)

    val reservation1 = lrcTokenReserveManager.reservations(0)
    reservation1.orderId should be(order1.id)
    reservation1.accumulatedBalance should be(50)
    reservation1.accumulatedAllowance should be(50)

    val reservation2 = lrcTokenReserveManager.reservations(1)
    reservation2.orderId should be(order2.id)
    reservation2.accumulatedBalance should be(80)
    reservation2.accumulatedAllowance should be(80)

    val reservation3 = lrcTokenReserveManager.reservations(2)
    reservation3.orderId should be(order3.id)
    reservation3.accumulatedBalance should be(130)
    reservation3.accumulatedAllowance should be(130)
  }

  // 部分成交2:
  // 连续下三个订单,tokenS对应的balance充足,allowance在order2变得不充足
  // order1成交30(剩余20)后,tokenManager的订单链位置应该不受影响
  // cursor应该仍然为2,
  // reservation数量不变,accumulatedAmount从order2递减
  // balance&allowance不变
  // availableBalance&availableAllowance对应增加order2成交量
  "complexTest2" should "submit orders then fill one of them" in {
    val (manager, orderPool, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager) = prepare

    lrcTokenReserveManager.init(200, 100)
    xyzTokenReserveManager.init(200, 200)
    gtoTokenReserveManager.init(200, 200)

    val order1 = Order(
      "order1",
      lrc,
      xyz,
      gto,
      50,
      10,
      10
    )
    manager.submitOrder(order1)

    val order2 = Order(
      "order2",
      lrc,
      xyz,
      gto,
      100,
      10,
      10
    )
    manager.submitOrder(order2)

    val order3 = Order(
      "order3",
      lrc,
      xyz,
      gto,
      50,
      10,
      10
    )
    manager.submitOrder(order3)

    // order1成交30, 剩余20
    manager.adjustOrder(order1.id, 20)

    lrcTokenReserveManager.balance should be(200)
    lrcTokenReserveManager.allowance should be(100)

    lrcTokenReserveManager.availableBalance should be(30)
    lrcTokenReserveManager.availableAllowance should be(0)

    lrcTokenReserveManager.cursor should be(2)

    lrcTokenReserveManager.idxMap.size should be(3)
    lrcTokenReserveManager.idxMap.getOrElse(order1.id, -1) should be(0)
    lrcTokenReserveManager.idxMap.getOrElse(order2.id, -1) should be(1)
    lrcTokenReserveManager.idxMap.getOrElse(order3.id, -1) should be(2)

    lrcTokenReserveManager.reservations.size should be(3)

    val reservation1 = lrcTokenReserveManager.reservations(0)
    reservation1.orderId should be(order1.id)
    reservation1.accumulatedBalance should be(20)
    reservation1.accumulatedAllowance should be(20)

    val reservation2 = lrcTokenReserveManager.reservations(1)
    reservation2.orderId should be(order2.id)
    reservation2.accumulatedBalance should be(120)
    reservation2.accumulatedAllowance should be(100)

    val reservation3 = lrcTokenReserveManager.reservations(2)
    reservation3.orderId should be(order3.id)
    reservation3.accumulatedBalance should be(170)
    reservation3.accumulatedAllowance should be(100)
  }

  // 完全成交
}
