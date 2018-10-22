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

class AccumulatedAmountSpec extends FlatSpec with Matchers {

  import Helper._

  info("[sbt core/'testOnly *AccumulatedAmountSpec']")

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

  // 简单的测试: 下两个订单,总金额不超过账户总金额
  // reservation应该是两个订单balance/allowance之和
  "simpleTest" should "with the same amount" in {
    lrcTokenManager.init(100, 100)
    xyzTokenManager.init(100, 100)
    gtoTokenManager.init(100, 100)

    manager.submitOrder(newOrder(
      "order1",
      lrc,
      xyz,
      None,
      20,
      10,
      10
    ))

    manager.submitOrder(newOrder(
      "order2",
      lrc,
      xyz,
      None,
      30,
      10,
      20
    ))

    lrcTokenManager.reservations.head.accumulatedBalance should be(30)
    lrcTokenManager.reservations.last.accumulatedBalance should be(80)
  }
}
