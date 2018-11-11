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

class OrderManagerImplSpec_Adjustment extends CommonSpec {
  "adjustment of single order upward and downward" should "just work" in {
    dai.init(1000, 1000)
    lrc.init(1000, 1000)

    val order1 = sellDAI(100, 10, 40)
    orderManager.submitOrder(order1) should be(true)
    orderPool.size should be(1)

    updatedOrders = Map.empty[String, Order]
    orderManager.adjustOrder(order1.id, BigInt(40))
    updatedOrders.size should be(1)

    {
      val order = updatedOrders(order1.id)
      order.status should be(OrderStatus.PENDING)
      order.reserved should be(orderState(40, 0, 16))
      order.actual should be(orderState(40, 4, 16))
    }

    updatedOrders = Map.empty[String, Order]
    orderManager.adjustOrder(order1.id, BigInt(140))
    updatedOrders.size should be(1)

    {
      val order = updatedOrders(order1.id)
      order.status should be(OrderStatus.PENDING)
      order.reserved should be(orderState(100, 0, 40))
      order.actual should be(orderState(100, 10, 40))
    }
  }

  "adjustment of the last order upward and downward" should "just work" in {
    dai.init(1000, 1000)
    lrc.init(1000, 1000)

    val order1 = sellDAI(200, 20, 200)
    orderManager.submitOrder(order1) should be(true)

    val order2 = sellDAI(100, 10, 40)
    orderManager.submitOrder(order2) should be(true)
    orderPool.size should be(2)

    updatedOrders = Map.empty[String, Order]
    orderManager.adjustOrder(order2.id, BigInt(40))
    updatedOrders.size should be(1)

    {
      val order = updatedOrders(order2.id)
      order.status should be(OrderStatus.PENDING)
      order.reserved should be(orderState(40, 0, 16))
      order.actual should be(orderState(40, 4, 16))
    }

    updatedOrders = Map.empty[String, Order]
    orderManager.adjustOrder(order2.id, BigInt(140))
    updatedOrders.size should be(1)

    {
      val order = updatedOrders(order2.id)
      order.status should be(OrderStatus.PENDING)
      order.reserved should be(orderState(100, 0, 40))
      order.actual should be(orderState(100, 10, 40))
    }
  }

  "adjustment of the first order upward and downward" should "just work" in {

  }
}
