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

class RingMatcherImplSpec_AmountCalculation extends CommonSpec {

  val ringIncomeEstimator = new RingIncomeEstimatorImpl()

  val nonProfitable = new RingIncomeEstimator {
    def getRingIncome(ring: OrderRing) = 0

    def isProfitable(ring: OrderRing, fiatValueThreshold: Double) = false
  }

  val alwaysProfitable = new RingIncomeEstimator {
    def getRingIncome(ring: OrderRing) = Long.MaxValue

    def isProfitable(ring: OrderRing, fiatValueThreshold: Double) = true
  }

  var decimal = ""
  decimal += "0"
  (0 until 18).foreach(i ⇒ decimal += "0")

  "RingMatcherImpl" +
    "" should "calculate volume in case of price just match " in {
      implicit val incomeEstimator = alwaysProfitable
      val matcher = new RingMatcherImpl()
      val (makerAmountS, makerAmountB, makerFee) = (BigInt("100000000000" + decimal), BigInt("10000000000" + decimal), BigInt("10" + decimal))
      val (takerAmountS, takerAmountB, takerFee) = (BigInt("10000000000" + decimal), BigInt("100000000000" + decimal), BigInt("10" + decimal))
      val maker = sellDAI(makerAmountS, makerAmountB, makerFee)
      val taker = buyDAI(takerAmountS, takerAmountB, takerFee)

      info("matchable is raw amount")
      val res = matcher.matchOrders(
        taker.copy(_matchable = Some(OrderState(amountS = takerAmountS, amountB = takerAmountB, amountFee = takerFee))),
        maker.copy(_matchable = Some(OrderState(amountS = makerAmountS, amountB = makerAmountB, amountFee = makerFee)))
      )
      val expectRing = OrderRing(
        taker = ExpectedFill(
          order = taker.copy(_matchable = Some(OrderState())),
          pending = OrderState(amountS = takerAmountS, amountB = takerAmountB, amountFee = takerFee),
          amountMargin = BigInt(0)
        ),
        maker = ExpectedFill(
          order = maker.copy(_matchable = Some(OrderState())),
          pending = OrderState(amountS = makerAmountS, amountB = makerAmountB, amountFee = makerFee),
          amountMargin = BigInt(0)
        )
      )
      res.right should be(Some(expectRing))

      info("both matchables are half of raw amount")
      val res1 = matcher.matchOrders(
        taker.copy(_matchable = Some(OrderState(amountS = takerAmountS / 2, amountB = takerAmountB / 2, amountFee = takerFee / 2))),
        maker.copy(_matchable = Some(OrderState(amountS = makerAmountS / 2, amountB = makerAmountB / 2, amountFee = makerFee / 2)))
      )
      val expectRing1 = OrderRing(
        taker = ExpectedFill(
          order = taker.copy(_matchable = Some(OrderState())),
          pending = OrderState(amountS = takerAmountS / 2, amountB = takerAmountB / 2, amountFee = takerFee / 2),
          amountMargin = BigInt(0)
        ),
        maker = ExpectedFill(
          order = maker.copy(_matchable = Some(OrderState())),
          pending = OrderState(amountS = makerAmountS / 2, amountB = makerAmountB / 2, amountFee = makerFee / 2),
          amountMargin = BigInt(0)
        )
      )
      res1.right should be(Some(expectRing1))

      info("both matchables are a third of raw amount")
      val res4 = matcher.matchOrders(
        taker.copy(_matchable = Some(OrderState(amountS = takerAmountS / 3, amountB = takerAmountB / 3, amountFee = takerFee / 3))),
        maker.copy(_matchable = Some(OrderState(amountS = makerAmountS / 3, amountB = makerAmountB / 3, amountFee = makerFee / 3)))
      )
      val expectRing4 = OrderRing(
        taker = ExpectedFill(
          order = taker.copy(_matchable = Some(OrderState())),
          pending = OrderState(amountS = takerAmountS / 3, amountB = takerAmountB / 3, amountFee = takerFee / 3),
          amountMargin = BigInt(0)
        ),
        maker = ExpectedFill(
          order = maker.copy(_matchable = Some(OrderState())),
          pending = OrderState(amountS = makerAmountS / 3, amountB = makerAmountB / 3, amountFee = makerFee / 3),
          amountMargin = BigInt(0)
        )
      )
      res4.right should be(Some(expectRing4))

      info("one of the matchables are half of raw amount")
      val res2 = matcher.matchOrders(
        taker.copy(_matchable = Some(OrderState(amountS = takerAmountS / 2, amountB = takerAmountB / 2, amountFee = takerFee / 2))),
        maker.copy(_matchable = Some(OrderState(amountS = makerAmountS, amountB = makerAmountB, amountFee = makerFee)))
      )

      val expectRing2 = OrderRing(
        taker = ExpectedFill(
          order = taker.copy(_matchable = Some(OrderState())),
          pending = OrderState(amountS = takerAmountS / 2, amountB = takerAmountB / 2, amountFee = takerFee / 2),
          amountMargin = BigInt(0)
        ),
        maker = ExpectedFill(
          order = maker.copy(_matchable = Some(OrderState(
            amountS = makerAmountS - takerAmountB / 2,
            amountB = makerAmountB - takerAmountS / 2,
            amountFee = Rational(makerFee) - Rational(makerFee * (makerAmountS - takerAmountB / 2), makerAmountS)
          ))),
          pending = OrderState(
            amountS =
              takerAmountB / 2,
            amountB = takerAmountS / 2,
            amountFee = Rational(makerFee * (makerAmountS - takerAmountB / 2), makerAmountS)
          ),
          amountMargin = BigInt(0)
        )
      )
      res2.right should be(Some(expectRing2))
    }

  "RingMatcherImpl1" should "calulate volume in case of price with margin " in {
    implicit val incomeEstimator = alwaysProfitable
    val matcher = new RingMatcherImpl()
    val (makerAmountS, makerAmountB, makerFee) = (BigInt("1500000000000" + decimal), BigInt("90000000000" + decimal), BigInt("90" + decimal))
    val (takerAmountS, takerAmountB, takerFee) = (BigInt("10000000000" + decimal), BigInt("100000000000" + decimal), BigInt("10" + decimal))
    val maker = sellDAI(makerAmountS, makerAmountB, makerFee)
    val taker = buyDAI(takerAmountS, takerAmountB, takerFee)

    info("matchable is raw amount")
    val res = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = takerAmountS, amountB = takerAmountB, amountFee = takerFee))),
      maker.copy(_matchable = Some(OrderState(amountS = makerAmountS, amountB = makerAmountB, amountFee = makerFee)))
    )
    val expectRing = OrderRing(
      taker = ExpectedFill(
        order = taker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = takerAmountS, amountB = takerAmountB, amountFee = takerFee),
        amountMargin = BigInt("4000000000" + decimal)
      ),
      maker = ExpectedFill(
        order = maker.copy(_matchable = Some(OrderState(amountS = BigInt("1400000000000" + decimal), amountB = BigInt("84000000000" + decimal), amountFee = BigInt("84" + decimal)))),
        pending = OrderState(amountS = BigInt("100000000000" + decimal), amountB = BigInt("6000000000" + decimal), amountFee = BigInt("6" + decimal)),
        amountMargin = BigInt(0)
      )
    )
    res.right should be(Some(expectRing))

    info("both matchables are half of raw amount")
    val res1 = matcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = takerAmountS / 2, amountB = takerAmountB / 2, amountFee = takerFee / 2))),
      maker.copy(_matchable = Some(OrderState(amountS = makerAmountS / 2, amountB = takerAmountB / 2, amountFee = takerFee / 2)))
    )
    val expectRing1 = OrderRing(
      taker = ExpectedFill(
        order = taker.copy(_matchable = Some(OrderState())),
        pending = OrderState(amountS = takerAmountS / 2, amountB = takerAmountB / 2, amountFee = takerFee / 2),
        amountMargin = BigInt("2000000000" + decimal)
      ),
      maker = ExpectedFill(
        order = maker.copy(_matchable = Some(OrderState(amountS = BigInt("700000000000" + decimal), amountB = BigInt("47000000000" + decimal), amountFee = BigInt("46666666666666666667")))),
        pending = OrderState(amountS = BigInt("50000000000" + decimal), amountB = BigInt("3000000000" + decimal), amountFee = BigInt("3333333333333333333")),
        amountMargin = BigInt(0)
      )
    )
    res1.right should be(Some(expectRing1))
  }
}
