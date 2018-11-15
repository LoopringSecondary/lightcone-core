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

package org.loopring.lightcone.core.market

import org.loopring.lightcone.core.base._
import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core._
import OrderStatus._
// var marketId = MarketId(GTO, WETH)
// var config = MarketManagerConfig(
//   maxNumbersOfOrders = 100, /* unsupported */
//   priceDecimals = 5
// )

// var fackRingMatcher: RingMatcher = _
// var fakeDustOrderEvaluator: DustOrderEvaluator = _
// var fakePendingRingPool: PendingRingPool = _
// var marketManager: MarketManager = _

class MarketManagerImplSpec_Basic extends MarketAwareSpec {

  "MarketManager" should "reject orders whose original size is dust" in {
    var order = sellGTO(1000, 1)
    (fakeDustOrderEvaluator.isOriginalDust _).when(order).returns(true)

    marketManager.submitOrder(order, 0) should be(
      MarketManager.MatchResult(
        Nil,
        order.copy(status = OrderStatus.DUST_ORDER),
        OrderbookUpdate()
      )
    )
    noMatchingActivity()
  }

  "MarketManager" should "reject orders whose actual size is dust" in {
    var order = sellGTO(1000, 1)
    (fakeDustOrderEvaluator.isOriginalDust _).when(order).returns(false)
    (fakeDustOrderEvaluator.isActualDust _).when(order).returns(true)

    val result = marketManager.submitOrder(order, 0)
    result should be(emptyMatchingResult(order, COMPLETELY_FILLED))

    noMatchingActivity()
  }

  "MarketManager" should "accept sell orders" in {
    var order1 = notDust(sellGTO(100000, 100)) // price =  100000/100.0 = 1000.00
    var order2 = notDust(sellGTO(100000, 101)) // price =  100000/101.0 = 989.12

    (fakePendingRingPool.getOrderPendingAmountS _).when(*).returns(0)
    (fakeAggregator.getOrderbookUpdate _).when(0).returns(OrderbookUpdate())

    var result = marketManager.submitOrder(order1, 0)
    result should be(emptyMatchingResult(order1, PENDING))

    result = marketManager.submitOrder(order2, 0)
    result should be(emptyMatchingResult(order2, PENDING))

    noMatchingActivity()

    marketManager.getNumOfSellOrders() should be(2)
    marketManager.getNumOfBuyOrders() should be(0)
    marketManager.getNumOfOrders() should be(2)

    marketManager.getSellOrders(3) should be(Seq(
      order2.copy(status = PENDING),
      order1.copy(status = PENDING)
    ))

    marketManager.getSellOrders(1) should be(Seq(
      order2.copy(status = PENDING)
    ))

    marketManager.getBuyOrders(100) should be(Nil)
  }

  "MarketManager" should "accept buy orders" in {
    var order1 = notDust(buyGTO(100, 100000)) // price =  100000/100.0 = 1000.00
    var order2 = notDust(buyGTO(101, 100000)) // price =  100000/101.0 = 989.12

    (fakePendingRingPool.getOrderPendingAmountS _).when(*).returns(0)
    (fakeAggregator.getOrderbookUpdate _).when(0).returns(OrderbookUpdate())

    var result = marketManager.submitOrder(order1, 0)
    result should be(emptyMatchingResult(order1, PENDING))

    result = marketManager.submitOrder(order2, 0)
    result should be(emptyMatchingResult(order2, PENDING))

    noMatchingActivity()

    marketManager.getNumOfSellOrders() should be(0)
    marketManager.getNumOfBuyOrders() should be(2)
    marketManager.getNumOfOrders() should be(2)

    marketManager.getBuyOrders(3) should be(Seq(
      order1.copy(status = PENDING),
      order2.copy(status = PENDING)
    ))

    marketManager.getBuyOrders(1) should be(Seq(
      order1.copy(status = PENDING)
    ))

    marketManager.getSellOrders(100) should be(Nil)
  }

  //--------

  private def notDust(order: Order): Order = {
    (fakeDustOrderEvaluator.isOriginalDust _).when(order).returns(false)
    (fakeDustOrderEvaluator.isActualDust _).when(order).returns(false)
    order
  }

  private def emptyMatchingResult(order: Order, newStatus: OrderStatus) =
    MarketManager.MatchResult(Nil, order.copy(status = newStatus), OrderbookUpdate())

  private def noMatchingActivity() = {
    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .verify(*, *, *).never
  }
}
