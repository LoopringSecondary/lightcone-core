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

    // verify matching is not involved
    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .verify(*, *, *).never
  }

  "MarketManager" should "reject orders whose actual size is dust" in {
    var order = sellGTO(1000, 1)
    (fakeDustOrderEvaluator.isOriginalDust _).when(order).returns(false)
    (fakeDustOrderEvaluator.isActualDust _).when(order).returns(true)

    marketManager.submitOrder(order, 0) should be(emptyMatchingResult(order, COMPLETELY_FILLED))

    // verify matching is not involved
    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .verify(*, *, *).never
  }

  "MarketManager" should "accept a single sell order" in {
    var order1 = notDust(sellGTO(100000, 100))
    var order2 = notDust(sellGTO(100000, 101))
    (fakePendingRingPool.getOrderPendingAmountS _).when(*).returns(0)

    marketManager.submitOrder(order1, 0) should be(emptyMatchingResult(order1, PENDING))
    marketManager.submitOrder(order2, 0) should be(emptyMatchingResult(order2, PENDING))

    // verify matching is not involved
    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .verify(*, *, *).never

  }

  private def notDust(order: Order): Order = {
    (fakeDustOrderEvaluator.isOriginalDust _).when(order).returns(false)
    (fakeDustOrderEvaluator.isActualDust _).when(order).returns(false)
    order
  }

  private def emptyMatchingResult(order: Order, newStatus: OrderStatus) =
    MarketManager.MatchResult(Nil, order.copy(status = newStatus), OrderbookUpdate())
}
