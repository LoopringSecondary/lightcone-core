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

class OrderManagerImplSpec_Basic extends CommonSpec {
  "submit order" should "fail when tokenS balance is low" in {
    dai.init(0, 0)
    val order = sellDAI(100, 1)
    submitOrder(order) should be(false)
    orderPool.size should be(0)
    updatedOrders(order.id).status should be(OrderStatus.CANCELLED_LOW_BALANCE)
  }

  "submit order" should "fail when tokenFee balance is low" in {
    dai.init(100, 100)
    val order = sellDAI(100, 1000, 10)
    submitOrder(order) should be(false)
    orderPool.size should be(0)
    updatedOrders(order.id).status should be(OrderStatus.CANCELLED_LOW_FEE_BALANCE)
  }

  "submit order" should "succeed when order consumes all tokenS but only part of tokenFee" in {
    dai.init(100, 100)
    lrc.init(100, 100)
    val order = sellDAI(100, 1000, 10)
    submitOrder(order) should be(true)
    orderPool.size should be(1)
    val updated = orderPool(order.id)
    updatedOrders(order.id) should be(updated)

    val reserved = updated.reserved
    val actual = updated.actual

    reserved should be(orderState(100, 0, 10))
    actual should be(orderState(100, 1000, 10))
  }

  "submit order" should "succeed when order consumes all tokenFee but only part of tokenS" in {
    dai.init(100, 100)
    lrc.init(100, 100)
    val order = sellDAI(50, 1000, 100)
    submitOrder(order) should be(true)
    orderPool.size should be(1)
    val updated = orderPool(order.id)
    updatedOrders(order.id) should be(updated)

    val reserved = updated.reserved
    val actual = updated.actual

    reserved should be(orderState(50, 0, 100))
    actual should be(orderState(50, 1000, 100))
  }

  "submit order" should "succeed when order consumes only part of tokenS and tokenFee" in {
    dai.init(100, 100)
    lrc.init(100, 100)
    val order = sellDAI(10, 1000, 20)
    submitOrder(order) should be(true)
    orderPool.size should be(1)
    val updated = orderPool(order.id)
    updatedOrders(order.id) should be(updated)

    val reserved = updated.reserved
    val actual = updated.actual

    reserved should be(orderState(10, 0, 20))
    actual should be(orderState(10, 1000, 20))
  }

  "submit order" should "fail if amountS is 0" in {
    dai.init(100, 100)
    lrc.init(100, 100)
    val order = sellDAI(0, 1000, 20)
    submitOrder(order) should be(false)
    orderPool.size should be(0)
    updatedOrders(order.id).status should be(OrderStatus.INVALID_DATA)
  }

  "submit order" should "fail if tokenS is not supported" in {
    dai.init(100, 100)
    lrc.init(100, 100)
    val order = Order("id", "XYZ", WETH, LRC, BigInt(10), BigInt(10), BigInt(10))
    submitOrder(order) should be(false)
    orderPool.size should be(0)
    updatedOrders(order.id).status should be(OrderStatus.UNSUPPORTED_MARKET)
  }

  "submit order" should "fail if tokenFee is not supported" in {
    dai.init(100, 100)
    lrc.init(100, 100)
    val order = Order("id", DAI, WETH, "XYZ", BigInt(10), BigInt(10), BigInt(10))
    submitOrder(order) should be(false)
    orderPool.size should be(0)
    updatedOrders(order.id).status should be(OrderStatus.UNSUPPORTED_MARKET)
  }

  "cancel order" should "fail if the order does not exist" in {
    cancelOrder("bad-id") should be(false)
    updatedOrders.size should be(0)
  }
}
