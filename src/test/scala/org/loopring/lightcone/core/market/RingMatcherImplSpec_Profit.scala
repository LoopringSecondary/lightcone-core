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

class RingMatcherImplSpec_Profit extends CommonSpec {

  val maker = sellDAI(10, 10).copy(_matchable = Some(OrderState(10, 10)))
  val taker = buyDAI(10, 10).copy(_matchable = Some(OrderState(10, 10)))

  "RingMatcherImpl" should "not match orders if the ring is not profitable" in {

    implicit val nonProfitable = new RingIncomeEstimator {
      def getRingIncome(ring: OrderRing) = 0
      def isProfitable(ring: OrderRing, fiatValueThreshold: Double) = false
    }
    val matcher = new RingMatcherImpl()
    matcher.matchOrders(taker, maker, 0) should be(Left(INCOME_TOO_SMALL))
  }

  "RingMatcherImpl" should "match orders if the ring is indeed profitable" in {

    implicit val alwaysProfitable = new RingIncomeEstimator {
      def getRingIncome(ring: OrderRing) = Long.MaxValue
      def isProfitable(ring: OrderRing, fiatValueThreshold: Double) = true
    }
    val matcher = new RingMatcherImpl()
    matcher.matchOrders(taker, maker, 0).isRight should be(true)
  }

}
