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

class MarketManagerSpec extends FlatSpec with Matchers {

  val lrc = "LRC"
  val eth = "ETH"

  implicit val tve = new TokenValueEstimatorImpl()
  tve.setMarketCaps(Map[Address, Double](lrc → 0.8, eth → 1400))
  tve.setTokens(Map[Address, BigInt](lrc → BigInt(1), eth → BigInt(1)))

  val incomeEvaluator = new RingIncomeEstimatorImpl(10)
  val simpleMatcher = new SimpleRingMatcher(incomeEvaluator)

  implicit val dustEvaluator = new DustOrderEvaluatorImpl(5)

  implicit val orderPool = new OrderPool()
  implicit val timeProvider = new SystemTimeProvider()
  implicit val pendingRingPool = new PendingRingPoolImpl()
  var marketManager = new MarketManagerImpl(
    MarketId(lrc, eth),
    MarketManagerConfig(0, 0),
    simpleMatcher
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
      createdAt = 1,
      _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
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
      createdAt = 2,
      _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
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
      createdAt = 3,
      _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
    )
    val res1 = marketManager.submitOrder(maker1)
    assert(res1.rings.isEmpty && res1.fullyMatchedOrderIds.isEmpty && res1.affectedOrders.size == 1)
    assert(res1.affectedOrders("maker1").matchable.amountS == maker1.amountS)
    val res2 = marketManager.submitOrder(maker2)
    assert(res2.rings.isEmpty && res2.fullyMatchedOrderIds.isEmpty && res2.affectedOrders.size == 1)
    assert(res2.affectedOrders("maker2").matchable.amountS == maker2.amountS)
    val res3 = marketManager.submitOrder(maker3)
    assert(res3.rings.isEmpty && res3.fullyMatchedOrderIds.isEmpty && res3.affectedOrders.size == 1)
    assert(res3.affectedOrders("maker3").matchable.amountS == maker3.amountS)

    assert(marketManager.bids.size == 3)
  }

  "submitOrder" should " fullfill in first order" in {
    assert(marketManager.bids.nonEmpty)
    val taker = Order(
      "taker1",
      eth,
      lrc,
      lrc,
      10,
      100,
      10,
      walletSplitPercentage = 0.2,
      createdAt = 4,
      _actual = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10))
    )

    val res = marketManager.submitOrder(taker)
    assert(res.rings.size == 1 && res.fullyMatchedOrderIds.size == 2 && res.affectedOrders.size == 2)
    assert(res.affectedOrders("taker1").matchable.amountS == 0)
    assert(res.affectedOrders("maker1").matchable.amountS == 0)
    assert(pendingRingPool.orderMap("taker1").pendingAmountS == 10)
    assert(pendingRingPool.orderMap("maker1").pendingAmountS == 100)

    val res1 = marketManager.submitOrder(taker)
    res1.affectedOrders("taker1").matchable should be(OrderState())
    assert(pendingRingPool.orderMap("taker1").pendingAmountS == 10)
    assert(pendingRingPool.orderMap("maker1").pendingAmountS == 100)
    assert(marketManager.bids.size == 2)
    assert(marketManager.asks.isEmpty)

  }

  "submitOrder" should " fullfill two order" in {
    assert(marketManager.bids.nonEmpty)
    val taker = Order(
      "taker2",
      eth,
      lrc,
      lrc,
      20,
      200,
      20,
      walletSplitPercentage = 0.2,
      _actual = Some(OrderState(amountS = 20, amountB = 200, amountFee = 20))
    )

    val res = marketManager.submitOrder(taker)
    assert(res.rings.size == 2 && res.fullyMatchedOrderIds.size == 3 && res.affectedOrders.size == 3)
    assert(res.affectedOrders("taker2").matchable.amountS == 0)
    assert(res.affectedOrders("maker2").matchable.amountS == 0)
    assert(res.affectedOrders("maker3").matchable.amountS == 0)
    assert(pendingRingPool.getOrderPendingAmountS("taker2") == 20)
    assert(pendingRingPool.getOrderPendingAmountS("maker2") == 100)
    assert(pendingRingPool.getOrderPendingAmountS("maker3") == 100)
    assert(marketManager.bids.isEmpty)
    assert(marketManager.asks.isEmpty)
  }

  "submitOrder" should " partfill" in {
    val maker4 = Order(
      id = "maker4",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 100,
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2,
      createdAt = 4,
      _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
    )
    val maker5 = Order(
      id = "maker5",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 200,
      amountB = 20,
      amountFee = 20,
      walletSplitPercentage = 0.2,
      createdAt = 5,
      _actual = Some(OrderState(amountS = 200, amountB = 20, amountFee = 20))
    )

    marketManager.submitOrder(maker4)
    marketManager.submitOrder(maker5)

    val taker = Order(
      "taker3",
      eth,
      lrc,
      lrc,
      20,
      200,
      20,
      walletSplitPercentage = 0.2,
      _actual = Some(OrderState(amountS = 20, amountB = 200, amountFee = 20))
    )

    val res = marketManager.submitOrder(taker)
    assert(res.rings.size == 2 && res.fullyMatchedOrderIds.size == 2 && res.affectedOrders.size == 3)
    assert(res.affectedOrders("taker3").matchable.amountS == 0)
    assert(res.affectedOrders("maker4").matchable.amountS == 0)
    assert(res.affectedOrders("maker5").matchable.amountS == 100)
    assert(pendingRingPool.getOrderPendingAmountS("taker3") == 20)
    assert(pendingRingPool.getOrderPendingAmountS("maker4") == 100)
    assert(pendingRingPool.getOrderPendingAmountS("maker5") == 100)
    assert(marketManager.bids.size == 1)
    assert(marketManager.asks.isEmpty)
  }

  "submitOrder" should " submit dust order should have no affect to manager" in {
    val maker6 = Order(
      id = "maker--6",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 5,
      amountB = 1,
      amountFee = 10,
      walletSplitPercentage = 0.2,
      createdAt = 6,
      _actual = Some(OrderState(amountS = 5, amountB = 1, amountFee = 10))
    )
    val res = marketManager.submitOrder(maker6)

    assert(res.rings.isEmpty && res.fullyMatchedOrderIds.size == 1 && res.affectedOrders.size == 1)
    assert(res.affectedOrders("maker--6").matchable.amountS == 0)
    assert(pendingRingPool.getOrderPendingAmountS("maker--6") == 0)
    assert(marketManager.bids.size == 1)
    assert(marketManager.asks.isEmpty)
  }

  "submitOrder" should " submit order with dust value after matched " in {
    val taker = Order(
      "taker4",
      eth,
      lrc,
      lrc,
      10,
      99,
      20,
      walletSplitPercentage = 0.2,
      _actual = Some(OrderState(amountS = 10, amountB = 99, amountFee = 20))
    )
    val res = marketManager.submitOrder(taker)

    assert(res.rings.size == 1 && res.fullyMatchedOrderIds.size == 2 && res.affectedOrders.size == 2)
    assert(res.affectedOrders("taker4").matchable.amountS == 0)
    assert(res.affectedOrders("maker5").matchable.amountS == 0)
    assert(pendingRingPool.getOrderPendingAmountS("taker4") == 10)
    //以maker、taker的顺序，最小订单应为taker，所以maker需要缩减，卖出应为199
    assert(pendingRingPool.getOrderPendingAmountS("maker5") == 199)
    assert(marketManager.bids.isEmpty)
    assert(marketManager.asks.isEmpty)
  }

  "submitHugeOrder" should "huge orders" in {
    marketManager = new MarketManagerImpl(
      MarketId(lrc, eth),
      MarketManagerConfig(0, 0),
      simpleMatcher
    )
    val startTime = System.currentTimeMillis()
    (0 until 100) foreach (
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
          _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
        )
        marketManager.submitOrder(maker)
      }
    )
    info(marketManager.bids.size.toString)
    info(marketManager.asks.size.toString)
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
      _actual = Some(OrderState(amountS = 8, amountB = 100, amountFee = 20))
    )

    val res = marketManager.submitOrder(taker)
    info(res.toString)
  }

  "removeOrder" should "remove order" in {
    marketManager = new MarketManagerImpl(
      MarketId(lrc, eth),
      MarketManagerConfig(0, 0),
      simpleMatcher
    )
    val startTime = System.currentTimeMillis()
    val size = 10
    (0 until size) foreach (
      i ⇒ {
        val maker = Order(
          id = "maker-" + i,
          tokenS = lrc,
          tokenB = eth,
          tokenFee = lrc,
          amountS = 100,
          amountB = 10,
          amountFee = 10,
          walletSplitPercentage = 0.2,
          _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
        )
        marketManager.submitOrder(maker)
      }
    )
    val order = Order(
      id = "maker-1",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 100,
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2,
      _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
    )
    val res = marketManager.deleteOrder(order)
    info(res.toString)
    assert(res)
    assert(marketManager.bids.size == size - 1)
    assert(marketManager.asks.isEmpty)
  }

  "triggerMatch" should "rematch orders" in {
    marketManager = new MarketManagerImpl(
      MarketId(lrc, eth),
      MarketManagerConfig(0, 0),
      simpleMatcher
    )
    val startTime = System.currentTimeMillis()
    val size = 10
    (0 until size) foreach (
      i ⇒ {
        val maker = Order(
          id = "maker-" + i,
          tokenS = lrc,
          tokenB = eth,
          tokenFee = lrc,
          amountS = 100,
          amountB = 10,
          amountFee = 10,
          walletSplitPercentage = 0.2,
          _matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10)),
          _actual = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))
        )
        val taker = Order(
          id = "taker-" + i,
          tokenS = eth,
          tokenB = lrc,
          tokenFee = lrc,
          amountS = 10,
          amountB = 100,
          amountFee = 10,
          walletSplitPercentage = 0.2,
          _matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10)),
          _actual = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10))
        )
        marketManager.bids.add(maker)
        marketManager.asks.add(taker)
      }
    )
    assert(marketManager.bids.size == size)
    assert(marketManager.asks.size == size)
    val res = marketManager.triggerMatch()
    assert(marketManager.bids.isEmpty)
    assert(marketManager.asks.isEmpty)

    assert(res.rings.size == size && res.fullyMatchedOrderIds.size == 2 * size && res.affectedOrders.size == 2 * size)
    (0 until size) foreach {
      i ⇒
        assert(res.affectedOrders("taker-" + i).matchable.amountS == 0)
        assert(res.affectedOrders("maker-" + i).matchable.amountS == 0)
        assert(pendingRingPool.getOrderPendingAmountS("taker-" + i) == 10)
        assert(pendingRingPool.getOrderPendingAmountS("maker-" + i) == 100)
    }

    assert(marketManager.bids.isEmpty)
    assert(marketManager.asks.isEmpty)
  }
}
