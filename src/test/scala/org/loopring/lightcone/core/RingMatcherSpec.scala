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

class RingMatcherSpec extends FlatSpec with Matchers {

  val makerTokenS = "LRC"
  val takerTokenS = "ETH"

  implicit val tve = new TokenValueEstimator {
    val decimal = BigInt("1000000000000000000")
    override def getFiatValue(token: Address, amount: Amount): Double = {
      if (token == makerTokenS) {
        0.8 * (amount/decimal).doubleValue()
      } else {
        1400 * (amount/decimal).doubleValue()
      }
    }
    override def getBurnRate(token: Address): Double = {
      0.05
    }
  }

  val incomeEvaluator = new RingIncomeEstimatorImpl(10)
  val simpleMatcher = new SimpleRingMatcher(incomeEvaluator)

  "simpleMatcher" should "test matchOrders" in {
    info("[sbt core/'testOnly *RingMatcherSpec -- -z simpleMatcher']")

  }
  "incomeEvaluator" should "test fiatValue" in {
    info("[sbt core/'testOnly *RingMatcherSpec -- -z incomeEvaluator']")

  }

}
