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

class OrderStateManagerSpec extends FlatSpec with Matchers {

  case class Raw()

  type MyOrder = Order[Raw]
  type MyTokenManager = TokenManager[Raw]
  type MyOrderPool = OrderPool[Raw]

  "OrderStateManager" should "add new TokenManager" in {
    implicit val orderPool = new MyOrderPool()

    var receivedOrders = Map.empty[String, MyOrder]

    orderPool.addCallback(
      (order: MyOrder) ⇒ {
        receivedOrders += order.id -> order
      }
    )

    val manager = OrderStateManager.default[Raw]

    manager.hasTokenManager("LRC") should be(false)

    manager.addTokenManager(new MyTokenManager("LRC"))
    manager.hasTokenManager("LRC") should be(true)

    val lrc = manager.getTokenManager("LRC")
    lrc.reset(100, 200)

    val gto = manager.addTokenManager(new TokenManager("GTO"))
    gto.reset(5000, 4000)

    manager.submitOrder(Order(
      Raw(),
      "order1",
      10,
      "LRC"
    ))

    manager.submitOrder(Order(
      Raw(),
      "order2",
      15,
      "LRC",
      1
    ))

    manager.submitOrder(Order(
      Raw(),
      "order3",
      15,
      "LRC",
      10,
      Some("GTO")
    ))

    manager.submitOrder(Order(
      Raw(),
      "order4",
      150,
      "GTO",
      10,
      Some("LRC")
    ))

    manager.cancelOrder("order1")
    manager.cancelOrder("order2")
    manager.cancelOrder("order3")

    println("--------orderPool.orders")
    orderPool.orders.foreach(println)

    println("--------receivedOrders")
    receivedOrders.foreach(println)

    println("LRC =")
    println(lrc.getDebugInfo)

    println("GTO =")
    println(gto.getDebugInfo)
  }

}
