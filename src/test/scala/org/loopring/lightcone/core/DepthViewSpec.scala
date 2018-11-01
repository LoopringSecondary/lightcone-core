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

class DepthViewSpec extends FlatSpec with Matchers {

  "priceTest" should "get ask and bid price" in {
    info("[sbt core/'testOnly *DepthViewSpec -- -z priceTest']")

    implicit val orderPool = new DepthOrderPoolImpl()
    val marketId = MarketId(lrc, eth) // market == eth

    val granularity1 = Granularity(0.01, 2)
    val depth1 = new DepthView(marketId, granularity1)

    depth1.middlePrice(0.001) should be(0.01)
    depth1.middlePrice(0.02) should be(0.02)
    depth1.middlePrice(0.021) should be(0.03)
    depth1.middlePrice(0.029) should be(0.03)

    val granularity2 = Granularity(0.00000001d, 8)
    val depth2 = new DepthView(marketId, granularity2) // 10 -8次方

    depth2.middlePrice(0.0000000001d) should be(0.00000001d)
    depth2.middlePrice(120.00000002d) should be(120.00000002d)
    depth2.middlePrice(120.000000021d) should be(120.00000003d)
    depth2.middlePrice(120.000000029d) should be(120.00000003d)
  }

  "calculateBidsTest" should "calculate asks and bids" in {
    info("[sbt core/'testOnly *DepthViewSpec -- -z calculateBidsTest']")

    implicit val orderPool = new DepthOrderPoolImpl()
    val marketId = MarketId(lrc, eth) // market == eth

    val granularity = Granularity(0.01, 2)
    val depth = new DepthView(marketId, granularity, 3)

    val order1 = DepthOrder("1", lrc, eth, Rational(0.001), 50)
    depth.set(order1)
    depth.asks.size should be(0)
    depth.bids.size should be(1)
    depth.bids.get(0.01).map(_.amountS should be(50))
    orderPool.contains(order1.id) should be(true)

    val order2 = order1.copy(id = "2", price = Rational(0.031), amountS = 100)
    depth.set(order2)
    depth.asks.size should be(0)
    depth.bids.size should be(2)
    depth.bids.get(0.01).map(_.amountS should be(50))
    depth.bids.get(0.04).map(_.amountS should be(100))
    orderPool.contains(order2.id) should be(true)

    val order3 = order1.copy(id = "3", price = Rational(0.042), amountS = 100)
    depth.set(order3)
    depth.asks.size should be(0)
    depth.bids.size should be(3)
    depth.bids.get(0.01).map(_.amountS should be(50))
    depth.bids.get(0.04).map(_.amountS should be(100))
    depth.bids.get(0.05).map(_.amountS should be(100))
    orderPool.contains(order3.id) should be(true)

    val order4 = order1.copy(id = "4", price = Rational(0.022), amountS = 100)
    depth.set(order4)
    depth.asks.size should be(0)
    depth.bids.size should be(3)
    depth.bids.get(0.01).map(_.amountS should be(50))
    depth.bids.get(0.03).map(_.amountS should be(100))
    depth.bids.get(0.04).map(_.amountS should be(100))
    orderPool.contains(order4.id) should be(true)

    val order5 = order1.copy(id = "5", price = Rational(0.052), amountS = 100)
    depth.set(order5)
    depth.asks.size should be(0)
    depth.bids.size should be(3)
    depth.bids.get(0.01).map(_.amountS should be(50))
    depth.bids.get(0.03).map(_.amountS should be(100))
    depth.bids.get(0.04).map(_.amountS should be(100))
    orderPool.contains(order5.id) should be(true)

    val order6 = order1.copy(id = "6", price = Rational(0.012), amountS = 100)
    depth.set(order6)
    depth.asks.size should be(0)
    depth.bids.size should be(3)
    depth.bids.get(0.01).map(_.amountS should be(50))
    depth.bids.get(0.02).map(_.amountS should be(100))
    depth.bids.get(0.03).map(_.amountS should be(100))
    orderPool.contains(order6.id) should be(true)

    val order7 = order1.copy(id = "7", price = Rational(0.019), amountS = 100)
    depth.set(order7)
    depth.asks.size should be(0)
    depth.bids.size should be(3)
    depth.bids.get(0.01).map(_.amountS should be(50))
    depth.bids.get(0.02).map(_.amountS should be(200))
    depth.bids.get(0.03).map(_.amountS should be(100))
    orderPool.contains(order7.id) should be(true)
  }

  "calculateAsksTest" should "calculate asks and bids" in {
    info("[sbt core/'testOnly *DepthViewSpec -- -z calculateAsksTest']")

    implicit val orderPool = new DepthOrderPoolImpl()
    val marketId = MarketId(lrc, eth) // market == eth

    val granularity = Granularity(0.01, 2)
    val depth = new DepthView(marketId, granularity, 3)

    val order1 = DepthOrder("1", eth, lrc, Rational(0.001), 50)
    depth.set(order1)
    depth.bids.size should be(0)
    depth.asks.size should be(1)
    depth.asks.get(0.01).map(_.amountS should be(50))
    orderPool.contains(order1.id) should be(true)

    val order2 = order1.copy(id = "2", price = Rational(0.031), amountS = 100)
    depth.set(order2)
    depth.bids.size should be(0)
    depth.asks.size should be(2)
    depth.asks.get(0.01).map(_.amountS should be(50))
    depth.asks.get(0.04).map(_.amountS should be(100))
    orderPool.contains(order2.id) should be(true)

    val order3 = order1.copy(id = "3", price = Rational(0.042), amountS = 100)
    depth.set(order3)
    depth.bids.size should be(0)
    depth.asks.size should be(3)
    depth.asks.get(0.01).map(_.amountS should be(50))
    depth.asks.get(0.04).map(_.amountS should be(100))
    depth.asks.get(0.05).map(_.amountS should be(100))
    orderPool.contains(order3.id) should be(true)

    val order4 = order1.copy(id = "4", price = Rational(0.022), amountS = 100)
    depth.set(order4)
    depth.bids.size should be(0)
    depth.asks.size should be(3)
    depth.asks.get(0.01).map(_.amountS should be(50))
    depth.asks.get(0.03).map(_.amountS should be(100))
    depth.asks.get(0.04).map(_.amountS should be(100))
    orderPool.contains(order4.id) should be(true)

    val order5 = order1.copy(id = "5", price = Rational(0.052), amountS = 100)
    depth.set(order5)
    depth.bids.size should be(0)
    depth.asks.size should be(3)
    depth.asks.get(0.01).map(_.amountS should be(50))
    depth.asks.get(0.03).map(_.amountS should be(100))
    depth.asks.get(0.04).map(_.amountS should be(100))
    orderPool.contains(order5.id) should be(true)

    val order6 = order1.copy(id = "6", price = Rational(0.012), amountS = 100)
    depth.set(order6)
    depth.bids.size should be(0)
    depth.asks.size should be(3)
    depth.asks.get(0.01).map(_.amountS should be(50))
    depth.asks.get(0.02).map(_.amountS should be(100))
    depth.asks.get(0.03).map(_.amountS should be(100))
    orderPool.contains(order6.id) should be(true)

    val order7 = order1.copy(id = "7", price = Rational(0.019), amountS = 100)
    depth.set(order7)
    depth.bids.size should be(0)
    depth.asks.size should be(3)
    depth.asks.get(0.01).map(_.amountS should be(50))
    depth.asks.get(0.02).map(_.amountS should be(200))
    depth.asks.get(0.03).map(_.amountS should be(100))
    orderPool.contains(order7.id) should be(true)
  }

  "delOrderTest" should "del order while amountS is 0" in {
    info("[sbt core/'testOnly *DepthViewSpec -- -z delOrderTest']")

    implicit val orderPool = new DepthOrderPoolImpl()
    val marketId = MarketId(lrc, eth) // market == eth

    val granularity = Granularity(0.01, 2)
    val depth = new DepthView(marketId, granularity, 3)

    val order1 = DepthOrder("1", eth, lrc, Rational(0.001), 50)
    val order2 = order1.copy(id = "2", price = Rational(0.031), amountS = 100)
    depth.set(order1)
    depth.set(order2)
    orderPool.contains(order1.id) should be(true)
    orderPool.contains(order2.id) should be(true)

    val delOrder1 = order1.copy(amountS = 0)
    depth.set(delOrder1)
    orderPool.contains(order1.id) should be(false)
    orderPool.contains(order2.id) should be(true)

    val delOrder2 = order2.copy(amountS = 0)
    depth.set(delOrder2)
    orderPool.contains(order1.id) should be(false)
    orderPool.contains(order2.id) should be(false)
  }

  // 构造asks&bids以及交叉部分,
  // 交叉部分asks > bids 最终展示asks的交叉部分
  "getTest" should "get asks and bids" in {

    info("[sbt core/'testOnly *DepthViewSpec -- -z getTest']")

    implicit val orderPool = new DepthOrderPoolImpl()
    val granularity = Granularity(0.1d, 1)
    val marketId = MarketId(lrc, eth) // market == eth
    val depth = new DepthView(marketId, granularity)

    val order1 = DepthOrder("1", eth, lrc, Rational(1.15), 100)
    val order2 = order1.copy(id = "2", price = Rational(1.09), amountS = 100)
    val order3 = order1.copy(id = "3", price = Rational(0.93), amountS = 200)
    val order4 = order1.copy(id = "4", price = Rational(0.95), amountS = 100)

    depth.set(order1)
    depth.set(order2)
    depth.set(order3)
    depth.set(order4)

    depth.asks.size should be(3)
    depth.asks.get(1.2).map(_.amountS should be(100))
    depth.asks.get(1.1).map(_.amountS should be(100))
    depth.asks.get(1.0).map(_.amountS should be(300))

    val order11 = DepthOrder("11", lrc, eth, Rational(0.76), 100)
    val order21 = order11.copy(id = "21", price = Rational(0.86), amountS = 100)
    val order31 = order11.copy(id = "31", price = Rational(0.93), amountS = 100)
    val order41 = order11.copy(id = "41", price = Rational(0.93), amountS = 100)

    depth.set(order11)
    depth.set(order21)
    depth.set(order31)
    depth.set(order41)

    depth.bids.size should be(3)
    depth.bids.get(1.0).map(_.amountS should be(200))
    depth.bids.get(0.9).map(_.amountS should be(100))
    depth.bids.get(0.8).map(_.amountS should be(100))

    val (asks1, bids1) = depth.get(0.94, 10)
    asks1.size should be(3)
    asks1.get(1.2).map(_.amountS should be(100))
    asks1.get(1.1).map(_.amountS should be(100))
    asks1.get(1.0).map(_.amountS should be(300))
    bids1.size should be(2)
    bids1.get(0.9).map(_.amountS should be(100))
    bids1.get(0.8).map(_.amountS should be(100))

    val (asks2, bids2) = depth.get(0.94, 1)
    asks2.size should be(1)
    asks2.get(1.0).map(_.amountS should be(300))
    bids2.size should be(1)
    bids2.get(0.9).map(_.amountS should be(100))
  }

}