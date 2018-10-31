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

  "calculateTest" should "calculate asks and bids" in {
    info("[sbt core/'testOnly *DepthViewSpec -- -z calculateTest']")

    implicit val orderPool = new DepthOrderPoolImpl()
    val marketId = MarketId(lrc, eth) // market == eth

    val granularity = Granularity(0.01, 2)
    val depth = new DepthView(marketId, granularity, 3)

    val preOrder1 = DepthOrder("1", lrc, eth, Rational(0.01), 0)
    val nxtOrder1 = preOrder1.copy(amountS = 100)
    depth.calculate(preOrder1, nxtOrder1)

    depth.asks.size should be(0)
    depth.bids.size should be(1)
    depth.bids.head._2.amountS should be(100)
    orderPool.getOrder("1").get should be(nxtOrder1)
  }

  "simpleTest1" should "set new orders" in {

    info("[sbt core/'testOnly *DepthViewSpec -- -z simpleTest1']")

    implicit val orderPool = new DepthOrderPoolImpl()
    val granularity = Granularity(0.01d, 2)
    val marketId = MarketId(lrc, eth) // market == eth
    val depthView = new DepthView(marketId, granularity)

    info(depthView.middlePrice(0.023).toString)

    //    val orders = Seq[DepthOrder](
    //      DepthOrder("1", lrc, eth, Rational(0.01), 100),
    //      DepthOrder("2", lrc, eth, Rational(0.02), 100),
    //      DepthOrder("3", lrc, eth, Rational(0.03), 100)
    //    )
    //
    //    orders.map(depthView.set)
    //
    //    val (asks, bids) = depthView.get()
    //
    //    info("asks:" + asks.toString())
    //    info("bids:" + bids.toString())
  }

}
