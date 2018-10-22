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
import Helper._

class ReserveAmountSpec extends FlatSpec with Matchers {

  info("[sbt core/'testOnly *ReserveAmountSpec']")

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

  //  xyzTokenManager.init(1000, 200)
  //  gtoTokenManager.init(1000, 200)
  //  lrcTokenManager.init(1000, 200)

  // 情况1:tokenFee == None, allowance充足
  "simpleTest1" should "calculate reserve amount" in {
    val order = newOrder(
      "order",
      lrc,
      xyz,
      None,
      100,
      20,
      100
    )
    val state = order.withReservedAmount(200)(lrc)

    state.reserved.amountS should be(100)
    state.reserved.amountFee should be(100)
    state.reservedAmount()(lrc) should be(200)
  }

  // 情况2: tokenFee == None, allowance不足
  "simpleTest2" should "calculate reserve amount" in {
    val order = newOrder(
      "order",
      lrc,
      xyz,
      None,
      100,
      20,
      100
    )
    val state = order.withReservedAmount(100)(lrc)

    state.reserved.amountS should be(50)
    state.reserved.amountFee should be(50)
    state.reservedAmount()(lrc) should be(100)
  }

  // 情况3: tokenFee == tokenS, allowance充足
  "simpleTest3" should "calculate reserve amount" in {
    val order = newOrder(
      "order",
      lrc,
      xyz,
      Option(lrc),
      100,
      20,
      100
    )
    val state = order.withReservedAmount(200)(lrc)

    info(state.reserved.amountS.toString())
    info(state.reserved.amountFee.toString())
    info(state.reservedAmount()(lrc).toString())

    //    state.reserved.amountS should be(100)
    //    state.reserved.amountFee should be(100)
    //    state.reservedAmount()(lrc) should be(200)
  }

  // 情况4: tokenFee == tokenS, allowance不足
  //  "testReserveAmount4" should "calculate reserve amount" in {
  //    val order = newOrder(
  //      "order",
  //      lrc,
  //      xyz,
  //      Option(lrc),
  //      100,
  //      20,
  //      100
  //    )
  //    val state = order.withReservedAmount(100)(lrc)
  //
  //    state.reserved.amountS should be(50)
  //    state.reserved.amountFee should be(50)
  //    state.reservedAmount()(lrc) should be(100)
  //  }

  // 情况5: tokenFee == tokenB, allowance充足

  // 情况6: tokenFee == tokenB, allowance不足
}
