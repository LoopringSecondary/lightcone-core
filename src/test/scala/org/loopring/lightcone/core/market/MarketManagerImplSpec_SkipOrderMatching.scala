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
import MatchingFailure._

class MarketManagerImplSpec_SkipOrderMatching extends MarketAwareSpec {
  "MarketManager" should "skip non-profitable orders" in {
    val buy1 = actualNotDust(buyGTO(100, 100050)) // worst price
    val buy2 = actualNotDust(buyGTO(100, 100040)).copy(_actual = Some(OrderState(30, 50020, 0)))
    val buy3 = actualNotDust(buyGTO(100, 100030)).withActualAsOriginal() // best price

    (fakeDustOrderEvaluator.isMatchableDust _).when(*).returns(false)
    (fakePendingRingPool.getOrderPendingAmountS _).when(*).returns(0)
    (fakeAggregator.getOrderbookUpdate _).when(0).returns(OrderbookUpdate())

    marketManager.submitOrder(buy1, 0)
    marketManager.submitOrder(buy2, 0)
    marketManager.submitOrder(buy3, 0)

    marketManager.getBuyOrders(5) should be(Seq(
      buy3.copy(status = PENDING),
      buy2.copy(status = PENDING),
      buy1.copy(status = PENDING)
    ))

    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .when(*, buy3.asPending.withMatchableAsActual, *)
      .returns(Left(INCOME_TOO_SMALL))

    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .when(*, buy2.asPending.copy(_matchable =
        Some(OrderState(30, 100040 * 3 / 10, 0))), // scale actual based on original ratio
        *)
      .returns(Left(INCOME_TOO_SMALL))

    val ring = OrderRing(null, null)
    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .when(*, buy1.asPending.withMatchableAsActual().withActualAsOriginal(), *)
      .returns(Right(ring))

    // Submit a sell order as the taker
    val sell1 = actualNotDust(sellGTO(110000, 100))
    val result = marketManager.submitOrder(sell1, 0)

    result should be(MarketManager.MatchResult(
      Seq(ring),
      sell1.copy(status = PENDING),
      OrderbookUpdate()
    ))

    marketManager.getSellOrders(100) should be(Seq(
      sell1.copy(status = PENDING)
    ))

    marketManager.getBuyOrders(5) should be(Seq(
      buy3.copy(status = PENDING),
      buy2.copy(status = PENDING),
      buy1.copy(status = PENDING)
    ))

    (fackRingMatcher.matchOrders(_: Order, _: Order, _: Double))
      .verify(*, *, *)
      .repeated(3)
  }

}
