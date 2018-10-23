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

import org.loopring.lightcone.core.markets.DustEvaluator
import org.scalatest._

class MarketManagerSpec extends FlatSpec with Matchers {

  val lrc = "LRC"
  val eth = "ETH"

  implicit val tve = new TokenValueEstimator {
    val decimal = BigInt("1")

    override def getFiatValue(token: Address, amount: Amount): Double = {
      if (token == lrc) {
        0.8 * (amount / decimal).doubleValue()
      } else {
        1400 * (amount / decimal).doubleValue()
      }
    }

    override def getBurnRate(token: Address): Double = {
      0.05
    }
  }

  val incomeEvaluator = new RingIncomeEstimatorImpl(10)
  val simpleMatcher = new SimpleRingMatcher(incomeEvaluator)

  val dustEvaluator = new DustEvaluator {
    val threshold = 1
    override def isDust(order: Order): Boolean = {
      order.matchable.amountS <= threshold
    }
  }

  implicit val orderPool = new OrderPool()
  implicit val timeProvider = new SystemTimeProvider()
  implicit val pendingRingPool = new PendingRingPoolImpl()
  var marketManager = new MarketManagerImpl(
    MarketId(lrc, eth),
    MarketManagerConfig(0, 0, 0, 0),
    simpleMatcher,
    dustEvaluator
  )

  "submitOrder" should "add orders to marketManager" in {
    val maker1 = Order(
      id = "maker1",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 100,
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2,
      _matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
    )
    val maker2 = Order(
      id = "maker2",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 100,
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2,
      _matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
    )
    val maker3 = Order(
      id = "maker3",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 100,
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2,
      _matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
    )
    marketManager.submitOrder(maker1)
    marketManager.submitOrder(maker2)
    marketManager.submitOrder(maker3)

    assert(marketManager.bids.size == 3)
  }

  "submitOrder" should "case of fullfill in first order" in {
    assert(marketManager.bids.nonEmpty)
    val taker = Order(
      "taker",
      eth,
      lrc,
      lrc,
      10,
      100,
      10,
      walletSplitPercentage = 0.2,
      _matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10))
    )

    val res = marketManager.submitOrder(taker)
    info(res.toString)
    info(pendingRingPool.orderMap.toString())
    val res1 = marketManager.submitOrder(taker)
    info(res1.toString)
    assert(marketManager.bids.size == 2)
    assert(marketManager.asks.isEmpty)
    assert(res.rings.size == 1 && res.fullyMatchedOrderIds.size == 2)
  }

  //  "submitOrder" should "case of fullfill two order" in {
  //    assert(marketManager.bids.nonEmpty)
  //    val taker = Order(
  //      "taker",
  //      eth,
  //      lrc,
  //      lrc,
  //      20,
  //      200,
  //      20,
  //      walletSplitPercentage = 0.2,
  //      _matchable = Some(OrderState(amountS = 20, amountB = 200, amountFee = 20))
  //    )
  //
  //    val res = marketManager.submitOrder(taker)
  //    info(res.toString)
  //    assert(marketManager.bids.isEmpty)
  //    assert(marketManager.asks.isEmpty)
  //    assert(res.rings.size == 2 && res.fullyMatchedOrderIds.size == 3)
  //  }
  //
  //  "submitOrder" should "case of partfill" in {
  //    val maker4 = Order(
  //      id = "maker4",
  //      tokenS = lrc,
  //      tokenB = eth,
  //      tokenFee = lrc,
  //      amountS = 100,
  //      amountB = 10,
  //      amountFee = 10,
  //      walletSplitPercentage = 0.2,
  //      _matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
  //    )
  //    val maker5 = Order(
  //      id = "maker5",
  //      tokenS = lrc,
  //      tokenB = eth,
  //      tokenFee = lrc,
  //      amountS = 200,
  //      amountB = 20,
  //      amountFee = 20,
  //      walletSplitPercentage = 0.2,
  //      _matchable = Some(OrderState(amountS = 200, amountB = 20, amountFee = 20))
  //    )
  //
  //    marketManager.submitOrder(maker4)
  //    marketManager.submitOrder(maker5)
  //
  //    val taker = Order(
  //      "taker",
  //      eth,
  //      lrc,
  //      lrc,
  //      20,
  //      200,
  //      20,
  //      walletSplitPercentage = 0.2,
  //      _matchable = Some(OrderState(amountS = 20, amountB = 200, amountFee = 20))
  //    )
  //
  //    val res = marketManager.submitOrder(taker)
  //    info(res.toString)
  //    assert(marketManager.bids.size == 1)
  //    assert(marketManager.asks.isEmpty)
  //    info(res.rings.toString())
  //    assert(res.rings.size == 2 && res.fullyMatchedOrderIds.size == 2)
  //  }

  "submitHugeOrder" should "huge orders" in {
    marketManager = new MarketManagerImpl(
      MarketId(lrc, eth),
      MarketManagerConfig(0, 0, 0, 0),
      simpleMatcher,
      dustEvaluator
    )
    val startTime = System.currentTimeMillis()
    (0 until 10000) foreach (
      i ⇒ {
        val maker = Order(
          id = "maker" + i,
          tokenS = lrc,
          tokenB = eth,
          tokenFee = lrc,
          amountS = 100,
          amountB = 10,
          amountFee = 10,
          walletSplitPercentage = 0.2,
          _matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
        )
        marketManager.submitOrder(maker)
      }
    )
    info("time of submit 10000 :" + (System.currentTimeMillis() - startTime))
    val taker = Order(
      "taker",
      eth,
      lrc,
      lrc,
      8,
      100,
      20,
      walletSplitPercentage = 0.2,
      _matchable = Some(OrderState(amountS = 8, amountB = 100, amountFee = 20))
    )

    val res = marketManager.submitOrder(taker)
    info(res.toString)
  }

}
