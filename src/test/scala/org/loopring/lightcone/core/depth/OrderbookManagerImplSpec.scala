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

package org.loopring.lightcone.core.depth

import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.CommonSpec
import org.scalatest._

class OrderbookManagerImplSpec extends CommonSpec {
  var obm: OrderbookManager = _
  val config = OrderbookConfig(
    levels = 2,
    priceDecimals = 5,
    precisionForAmount = 2,
    precisionForTotal = 1
  )

  override def beforeEach() {
    obm = new OrderbookManagerImpl(config)
  }

  "OrderbookManagerSpec" should "return empty order book after initialized" in {
    obm.getOrderbook(0, 100) should be(Orderbook(Nil, Nil))
    obm.getOrderbook(1, 100) should be(Orderbook(Nil, Nil))
  }

  "OrderbookManagerSpec" should "process very small slot" in {
    obm.processUpdate(OrderbookUpdate(Seq(
      OrderbookSlot(1, 10, 100)
    ), Nil))

    obm.getOrderbook(0, 100) should be(Orderbook(Seq(
      OrderbookItem("0.00001", "10.00", "100.0")
    ), Nil))

    obm.getOrderbook(1, 100) should be(Orderbook(Seq(
      OrderbookItem("0.0001", "10.00", "100.0")
    ), Nil))
  }

  "OrderbookManagerSpec" should "skip 0 value slots" in {
    obm.processUpdate(OrderbookUpdate(Seq(
      OrderbookSlot(0, 10, 100)
    ), Seq(
      OrderbookSlot(0, 10, 100)
    )))
    obm.getOrderbook(0, 100) should be(Orderbook(Nil, Nil))
    obm.getOrderbook(1, 100) should be(Orderbook(Nil, Nil))
  }

  "OrderbookManagerSpec" should "process sell slot and round up" in {
    obm.processUpdate(OrderbookUpdate(Seq(
      OrderbookSlot(12344, 10, 100)
    ), Nil))
    obm.getOrderbook(0, 100) should be(Orderbook(Seq(
      OrderbookItem("0.12344", "10.00", "100.0")
    ), Nil))

    obm.getOrderbook(1, 100) should be(Orderbook(Seq(
      OrderbookItem("0.1235", "10.00", "100.0")
    ), Nil))
  }

  "OrderbookManagerSpec" should "process buy slot and round down" in {
    obm.processUpdate(OrderbookUpdate(Nil, Seq(
      OrderbookSlot(12344, 10, 100)
    )))
    obm.getOrderbook(0, 100) should be(Orderbook(Nil, Seq(
      OrderbookItem("0.12344", "10.00", "100.0")
    )))

    obm.getOrderbook(1, 100) should be(Orderbook(Nil, Seq(
      OrderbookItem("0.1234", "10.00", "100.0")
    )))
  }

  "OrderbookManagerSpec" should "process sell slot with new lower values" in {
    obm.processUpdate(OrderbookUpdate(Seq(
      OrderbookSlot(12344, 10, 100),
      OrderbookSlot(12345, 20, 200)
    ), Nil))

    obm.processUpdate(OrderbookUpdate(Seq(
      OrderbookSlot(12344, 5, 40),
      OrderbookSlot(12345, 10, 80)
    ), Nil))

    obm.getOrderbook(0, 100) should be(Orderbook(Seq(
      OrderbookItem("0.12344", "5.00", "40.0"),
      OrderbookItem("0.12345", "10.00", "80.0")
    ), Nil))

    obm.getOrderbook(1, 100) should be(Orderbook(Seq(
      OrderbookItem("0.1235", "15.00", "120.0")
    ), Nil))
  }

  "OrderbookManagerSpec" should "process buy slot with new lower values" in {
    obm.processUpdate(OrderbookUpdate(Nil, Seq(
      OrderbookSlot(12344, 10, 100),
      OrderbookSlot(12345, 20, 200)
    )))

    obm.processUpdate(OrderbookUpdate(Nil, Seq(
      OrderbookSlot(12344, 5, 40),
      OrderbookSlot(12345, 10, 80)
    )))

    obm.getOrderbook(0, 100) should be(Orderbook(Nil, Seq(
      OrderbookItem("0.12345", "10.00", "80.0"),
      OrderbookItem("0.12344", "5.00", "40.0")
    )))

    obm.getOrderbook(1, 100) should be(Orderbook(Nil, Seq(
      OrderbookItem("0.1234", "15.00", "120.0")
    )))
  }
}
