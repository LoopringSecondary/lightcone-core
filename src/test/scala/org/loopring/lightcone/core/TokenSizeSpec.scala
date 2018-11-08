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

class TokenSizeSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *TokenSizeSpec -- -z testTokenSize']")

  // tokenManager订单size测试
  "testTokenSize" should "add new TokenManager" in {
    val (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager) = prepare

    lrcTokenManager.init(1000, 1000)
    xyzTokenManager.init(1000, 1000)
    gtoTokenManager.init(1000, 1000)

    lrcTokenManager.init(100, 200)

    val order = Order(
      "order1",
      lrc,
      xyz,
      lrc,
      10,
      2,
      0
    )
    manager.submitOrder(order)

    orderPool.getOrder(order.id).get.status should be(OrderStatus.PENDING)
  }

}
