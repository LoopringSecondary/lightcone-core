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
import org.scalatest._
import MatchingFailure._

class RingMatcherImplSpec extends CommonSpec {

  implicit val alwaysProfitable = new RingIncomeEstimator {
    def getRingIncome(ring: OrderRing) = Long.MaxValue
    def isProfitable(ring: OrderRing, fiatValueThreshold: Double) = true
  }

  "RingMatcherImpl" should "not match untradable orders" in {
    val matcher = new RingMatcherImpl()

    val maker = sellDAI(100000000, 100000001)!
    val taker = buyDAI(100000000, 100000000)!

    matcher.matchOrders(taker, maker, 0) should be(Left(ORDERS_NOT_TRADABLE))
  }

  "RingMatcherImpl" should "not match orders if one of them has tokenB as 0 " in {
    val matcher = new RingMatcherImpl()

    matcher.matchOrders(
      sellDAI(10, 0)!,
      buyDAI(10, 10)!
    ) should be(Left(ORDERS_NOT_TRADABLE))

    matcher.matchOrders(
      sellDAI(10, 10)!,
      buyDAI(10, 0)!
    ) should be(Left(ORDERS_NOT_TRADABLE))

    matcher.matchOrders(
      sellDAI(10, 0)!,
      buyDAI(10, 0)!
    ) should be(Left(ORDERS_NOT_TRADABLE))
  }

  "RingMatcherImpl" should "not match orders if one of them has tokenS as 0 " in {
    val matcher = new RingMatcherImpl()

    matcher.matchOrders(
      sellDAI(0, 10)!,
      buyDAI(10, 10)!
    ) should be(Left(ORDERS_NOT_TRADABLE))

    matcher.matchOrders(
      sellDAI(10, 10)!,
      buyDAI(0, 10)!
    ) should be(Left(ORDERS_NOT_TRADABLE))

    matcher.matchOrders(
      sellDAI(0, 10)!,
      buyDAI(0, 10)!
    ) should be(Left(ORDERS_NOT_TRADABLE))
  }

  "RingMatcherImpl" should "not match orders if their `_matchable` fields are not set" in {
    val matcher = new RingMatcherImpl()

    matcher.matchOrders(
      sellDAI(10, 10)!,
      buyDAI(10, 10)!
    ).isRight should be(true)

    // match the same orders with `_matchable`
    matcher.matchOrders(
      sellDAI(10, 10),
      buyDAI(10, 10)
    ) should be(Left(INCOME_TOO_SMALL))
  }

}
