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

  info("[sbt core/'testOnly *ReleseOrderSpec']")

  // 提交订单后完全成交,看订单是否在tokenS&tokenFee manager中是否都删除了
  "simpleTest2" should "submit order and fill partitial" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(200, 200)
    xyzTokenManager.init(200, 200)
    gtoTokenManager.init(200, 200)

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
  }

}
