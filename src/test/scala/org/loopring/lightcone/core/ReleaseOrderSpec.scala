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

class ReleaseOrderSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *ReleaseOrderSpec']")

  // 用户下多个订单然后取消其中两个
  "simpleTest1" should "submit order and fill partitial" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(300, 300)
    xyzTokenManager.init(300, 300)
    gtoTokenManager.init(300, 300)

    val order1 = Order("order1", lrc, xyz, gto, 100, 100, 100)
    val order2 = Order("order2", lrc, xyz, gto, 100, 100, 100)
    val order3 = Order("order3", lrc, xyz, gto, 100, 100, 100)

    manager.submitOrder(order1)
    manager.submitOrder(order2)
    manager.submitOrder(order3)

    manager.cancelOrder(order1.id)
    manager.cancelOrder(order2.id)

    lrcTokenManager.availableBalance should be(200)
    lrcTokenManager.availableAllowance should be(200)
    gtoTokenManager.availableBalance should be(200)
    gtoTokenManager.availableAllowance should be(200)

    lrcTokenManager.idxMap.size should be(1)
    gtoTokenManager.idxMap.size should be(1)

    lrcTokenManager.cursor should be(0)
    gtoTokenManager.cursor should be(0)

    lrcTokenManager.reservations.size should be(1)
    gtoTokenManager.reservations.size should be(1)
    lrcTokenManager.reservations.head.orderId should be(order3.id)
    gtoTokenManager.reservations.head.orderId should be(order3.id)

    orderPool.contains(order1.id) should be(false)
    orderPool.contains(order2.id) should be(false)
    orderPool.contains(order3.id) should be(true)
  }

  // 单个订单完全成交,看订单是否在tokenS&tokenFee manager中是否都删除了
  "simpleTest2" should "submit order and fill partitial" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(200, 200)
    xyzTokenManager.init(200, 200)
    gtoTokenManager.init(200, 200)

    val order = Order("order", lrc, xyz, gto, 100, 100, 100)

    manager.submitOrder(order)
    manager.adjustOrder(order.id, 0)

    orderPool.contains(order.id) should be(false)

    lrcTokenManager.balance should be(200)
    lrcTokenManager.allowance should be(200)
    gtoTokenManager.balance should be(200)
    gtoTokenManager.allowance should be(200)

    lrcTokenManager.availableBalance should be(200)
    lrcTokenManager.availableAllowance should be(200)
    gtoTokenManager.availableBalance should be(200)
    gtoTokenManager.availableAllowance should be(200)

    lrcTokenManager.idxMap.size should be(0)
    gtoTokenManager.idxMap.size should be(0)

    lrcTokenManager.cursor should be(-1)
    gtoTokenManager.cursor should be(-1)

    lrcTokenManager.reservations.size should be(0)
    gtoTokenManager.reservations.size should be(0)
    orderPool.contains(order.id) should be(false)
  }

  // 多个订单其中一个完全成交, 看tokenmanager剩下的订单及账户信息是否正确
  "simpleTest3" should "submit orders and one of them finished" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(300, 300)
    xyzTokenManager.init(300, 300)
    gtoTokenManager.init(300, 300)

    val order1 = Order("order1", lrc, xyz, gto, 100, 100, 100)
    val order2 = Order("order2", lrc, xyz, gto, 100, 100, 100)
    val order3 = Order("order3", lrc, xyz, gto, 100, 100, 100)

    manager.submitOrder(order1)
    manager.submitOrder(order2)
    manager.submitOrder(order3)

    manager.adjustOrder(order2.id, 0)

    lrcTokenManager.availableBalance should be(100)
    lrcTokenManager.availableAllowance should be(100)
    gtoTokenManager.availableBalance should be(100)
    gtoTokenManager.availableAllowance should be(100)

    lrcTokenManager.idxMap.size should be(2)
    gtoTokenManager.idxMap.size should be(2)

    lrcTokenManager.cursor should be(1)
    gtoTokenManager.cursor should be(1)

    lrcTokenManager.reservations.size should be(2)
    gtoTokenManager.reservations.size should be(2)
    lrcTokenManager.reservations.head.orderId should be(order1.id)
    gtoTokenManager.reservations.head.orderId should be(order1.id)
    lrcTokenManager.reservations.last.orderId should be(order3.id)
    gtoTokenManager.reservations.last.orderId should be(order3.id)

    orderPool.contains(order1.id) should be(true)
    orderPool.contains(order2.id) should be(false)
    orderPool.contains(order3.id) should be(true)
  }

  // 模仿链上行为,多个订单其中一个完全成交,然后账户更新
  // 这种情况比较特殊,因为订单详情并不在orderManager中获取
  "simpleTest4" should "submit orders and one of them finished" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(300, 300)
    xyzTokenManager.init(300, 300)
    gtoTokenManager.init(300, 300)

    val order1 = Order("order1", lrc, xyz, gto, 100, 100, 100)
    val order2 = Order("order2", lrc, xyz, gto, 100, 100, 100)
    val order3 = Order("order3", lrc, xyz, gto, 100, 100, 100)

    manager.submitOrder(order1)
    manager.submitOrder(order2)
    manager.submitOrder(order3)

    manager.adjustOrder(order2.id, 0)
    lrcTokenManager.init(200, 200)
    gtoTokenManager.init(200, 200)

    lrcTokenManager.availableBalance should be(0)
    lrcTokenManager.availableAllowance should be(0)
    gtoTokenManager.availableBalance should be(0)
    gtoTokenManager.availableAllowance should be(0)

    lrcTokenManager.idxMap.size should be(2)
    gtoTokenManager.idxMap.size should be(2)

    lrcTokenManager.cursor should be(1)
    gtoTokenManager.cursor should be(1)

    lrcTokenManager.reservations.size should be(2)
    gtoTokenManager.reservations.size should be(2)
    lrcTokenManager.reservations.head.orderId should be(order1.id)
    gtoTokenManager.reservations.head.orderId should be(order1.id)
    lrcTokenManager.reservations.last.orderId should be(order3.id)
    gtoTokenManager.reservations.last.orderId should be(order3.id)

    orderPool.contains(order1.id) should be(true)
    orderPool.contains(order2.id) should be(false)
    orderPool.contains(order3.id) should be(true)
  }

  // 模仿软取消, 多个订单其中一个先取消, 然后订单成交, 账户更新
  "simpleTest5" should "submit orders and one of them finished" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(300, 300)
    xyzTokenManager.init(300, 300)
    gtoTokenManager.init(300, 300)

    val order1 = Order("order1", lrc, xyz, gto, 100, 100, 100)
    val order2 = Order("order2", lrc, xyz, gto, 100, 100, 100)
    val order3 = Order("order3", lrc, xyz, gto, 100, 100, 100)

    manager.submitOrder(order1)
    manager.submitOrder(order2)
    manager.submitOrder(order3)

    // 软取消
    manager.cancelOrder(order2.id)
    lrcTokenManager.availableBalance should be(100)
    lrcTokenManager.availableAllowance should be(100)

    // 成交
    manager.adjustOrder(order2.id, 50)
    lrcTokenManager.init(250, 250)
    gtoTokenManager.init(250, 250)

    lrcTokenManager.balance should be(250)
    lrcTokenManager.allowance should be(250)
    gtoTokenManager.balance should be(250)
    gtoTokenManager.allowance should be(250)

    lrcTokenManager.availableBalance should be(50)
    lrcTokenManager.availableAllowance should be(50)
    gtoTokenManager.availableBalance should be(50)
    gtoTokenManager.availableAllowance should be(50)

    lrcTokenManager.idxMap.size should be(2)
    gtoTokenManager.idxMap.size should be(2)

    lrcTokenManager.cursor should be(1)
    gtoTokenManager.cursor should be(1)

    lrcTokenManager.reservations.size should be(2)
    gtoTokenManager.reservations.size should be(2)
    lrcTokenManager.reservations.head.orderId should be(order1.id)
    gtoTokenManager.reservations.head.orderId should be(order1.id)
    lrcTokenManager.reservations.last.orderId should be(order3.id)
    gtoTokenManager.reservations.last.orderId should be(order3.id)

    orderPool.contains(order1.id) should be(true)
    orderPool.contains(order2.id) should be(false)
    orderPool.contains(order3.id) should be(true)
  }
}
