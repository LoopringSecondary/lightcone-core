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

class OrderManagerSpec extends FlatSpec with Matchers {

  "OrderManager" should "add new TokenManager" in {
    implicit val orderPool = new OrderPool()

    var receivedOrders = Map.empty[String, Order]

    orderPool.addCallback(
      (order: Order) ⇒ {
        receivedOrders += order.id -> order
      })

    val manager = OrderManager.default(maxNumOrders = 1000)

    manager.hasTokenManager("LRC") should be(false)

    manager.addTokenManager(new TokenManager("LRC"))
    manager.hasTokenManager("LRC") should be(true)

    val lrc = manager.getTokenManager("LRC")
    lrc.init(100, 200)

    val xyz = manager.addTokenManager(new TokenManager("XYZ"))
    val gto = manager.addTokenManager(new TokenManager("GTO"))
    gto.init(5000, 4000)

    manager.submitOrder(Order(
      "order1",
      "LRC",
      "XYZ",
      None,
      10,
      1,
      0))

    manager.submitOrder(Order(
      "order2",
      "LRC",
      "XYZ",
      None,
      15,
      1,
      1))

    manager.submitOrder(Order(
      "order3",
      "LRC",
      "XYZ",
      Some("GTO"),
      15,
      1,
      10))

    manager.submitOrder(Order(
      "order4",
      "GTO",
      "LRC",
      Some("LRC"),
      150,
      1,
      10))

    println("LRC =")
    println(lrc.getDebugInfo)

    println("--------orderPool.orders")
    orderPool.orders.foreach(println)

    println("--------receivedOrders")
    receivedOrders.foreach(println)

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
