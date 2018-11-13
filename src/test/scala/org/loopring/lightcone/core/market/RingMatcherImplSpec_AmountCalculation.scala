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

import org.loopring.lightcone.core.CommonSpec
import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.testkit._

class RingMatcherImplSpec_AmountCalculation extends CommonSpec with RingMatcherAssert {

  val ringIncomeEstimator = new RingIncomeEstimatorImpl()

  val nonProfitable = new RingIncomeEstimator {
    def getRingIncome(ring: OrderRing) = 0

    def isProfitable(ring: OrderRing, fiatValueThreshold: Double) = false
  }

  val alwaysProfitable = new RingIncomeEstimator {
    def getRingIncome(ring: OrderRing) = Long.MaxValue

    def isProfitable(ring: OrderRing, fiatValueThreshold: Double) = true
  }

  "RingMatcherImpl" should "calculate volume in case of price just match " in {
    implicit val incomeEstimator = alwaysProfitable
    val matcher = new RingMatcherImpl()
    val maker = sellDAI(100, 10, 10)
    val taker = buyDAI(10, 100, 10)

    val res1 = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 5, amountB = 50, amountFee = 5))),
      maker.copy(_matchable = Some(OrderState(amountS = 50, amountB = 5, amountFee = 5)))
    )

    info("matchable is raw amount")
    val res = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10))),
      maker.copy(_matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10)))
    )
    val expectRing = OrderRing(
      taker = ExpectedFill(
        order = taker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = 10, amountB = 100, amountFee = 10),
        amountMargin = BigInt(0)
      ),
      maker = ExpectedFill(
        order = maker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = 100, amountB = 10, amountFee = 10),
        amountMargin = BigInt(0)
      )
    )
    shouldRing(res, Some(expectRing))

    info("both matchables is half of raw amount")
    val expectRing1 = OrderRing(
      taker = ExpectedFill(
        order = taker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = 5, amountB = 50, amountFee = 5),
        amountMargin = BigInt(0)
      ),
      maker = ExpectedFill(
        order = maker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = 50, amountB = 5, amountFee = 5),
        amountMargin = BigInt(0)
      )
    )
    shouldRing(res1, Some(expectRing1))

    info("one of the matchables are half of raw amount")
    val res2 = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 5, amountB = 50, amountFee = 5))),
      maker.copy(_matchable = Some(OrderState(amountS = 80, amountB = 8, amountFee = 8)))
    )
    val expectRing2 = OrderRing(
      taker = ExpectedFill(
        order = taker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = 5, amountB = 50, amountFee = 5),
        amountMargin = BigInt(0)
      ),
      maker = ExpectedFill(
        order = maker.copy(_matchable = Some(OrderState(amountS = 30, amountB = 3, amountFee = 3))),
        pending = OrderState(amountS = 50, amountB = 5, amountFee = 5),
        amountMargin = BigInt(0)
      )
    )
    shouldRing(res2, Some(expectRing2))

  }

  "RingMatcherImpl" should "calulate volume in case of price with margin " in {
    implicit val incomeEstimator = alwaysProfitable
    val matcher = new RingMatcherImpl()

    val maker = sellDAI(1500, 90, 90)
    val taker = buyDAI(10, 100, 10)

    info("matchable is raw amount")
    val res = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10))),
      maker.copy(_matchable = Some(OrderState(amountS = 1500, amountB = 90, amountFee = 90)))
    )
    val expectRing = OrderRing(
      taker = ExpectedFill(
        order = taker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = 10, amountB = 100, amountFee = 10),
        amountMargin = BigInt(4)
      ),
      maker = ExpectedFill(
        order = maker.copy(_matchable = Some(OrderState(amountS = 1400, amountB = 84, amountFee = 84))),
        pending = OrderState(amountS = 100, amountB = 6, amountFee = 6),
        amountMargin = BigInt(0)
      )
    )
    shouldRing(res, Some(expectRing))

    info("both matchables are half of raw amount")
    val res1 = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 5, amountB = 50, amountFee = 5))),
      maker.copy(_matchable = Some(OrderState(amountS = 750, amountB = 45, amountFee = 45)))
    )
    val expectRing1 = OrderRing(
      taker = ExpectedFill(
        order = taker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = 5, amountB = 50, amountFee = 5),
        amountMargin = BigInt(2)
      ),
      maker = ExpectedFill(
        order = maker.copy(_matchable = Some(OrderState(amountS = 700, amountB = 42, amountFee = 42))),
        pending = OrderState(amountS = 50, amountB = 3, amountFee = 3),
        amountMargin = BigInt(0)
      )
    )
    shouldRing(res1, Some(expectRing1))
  }

  "RingMatcherImpl" should "calulate fee in case of only token fee" in {
    implicit val incomeEstimator = ringIncomeEstimator
    val matcher = new RingMatcherImpl()
    tmm.updatePrices(Map[String, Double](LRC → 1.0))
    tmm.updateBurnRate(LRC, 0.1)

    val maker = sellDAI(
      100,
      10,
      10
    )

    val taker = buyDAI(
      10,
      100,
      10
    )

    info("reduce the matchable then the fiatIncome should be 9")
    info("set minFiatValue=10, then the income is not enough:")
    var res = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 5, amountB = 50, amountFee = 5))),
      maker.copy(_matchable = Some(OrderState(amountS = 50, amountB = 5, amountFee = 5))),
      10
    )
    shouldIncomeTooSmall(res)

    info("set minFiatValue=10, then the income is enough:")
    res = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 5, amountB = 50, amountFee = 5))),
      maker.copy(_matchable = Some(OrderState(amountS = 50, amountB = 5, amountFee = 5))),
      9
    )
    shouldRing(res)

    //放开交易规模，收益足够
    info("info the income is enough when set matchable=amountS:")
    val res1 = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10))),
      maker.copy(_matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10))),
      10
    )
    shouldRing(res1)
  }

  "RingMatcherImpl" should "calulate fee in case of only margin" in {
    implicit val incomeEstimator = ringIncomeEstimator
    val matcher = new RingMatcherImpl()
    tmm.updatePrices(Map[String, Double](LRC → 1.0))
    tmm.updateBurnRate(LRC, 0.1)

    val maker = sellLRC(
      100,
      10,
      0
    )

    val taker = buyLRC(
      12,
      100,
      0
    )

    info("reduce the matchable then the fiatIncome should be 9")
    info("set minFiatValue=10, then the income is not enough:")
    var res = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 6, amountB = 50))),
      maker.copy(_matchable = Some(OrderState(amountS = 50, amountB = 5))),
      10
    )
    shouldIncomeTooSmall(res)

    info("set minFiatValue=9, then the income is enough:")
    res = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 6, amountB = 50))),
      maker.copy(_matchable = Some(OrderState(amountS = 50, amountB = 5))),
      9
    )
    shouldRing(res)

    //放开交易规模，收益足够
    info("info the income is enough when set matchable=amountS:")
    val res1 = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 12, amountB = 100))),
      maker.copy(_matchable = Some(OrderState(amountS = 100, amountB = 10))),
      10
    )
    shouldRing(res1)
  }
}
