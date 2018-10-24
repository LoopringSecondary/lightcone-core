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

class RingIncomeEvaluatorSpec extends FlatSpec with Matchers {

  val lrc = "LRC"
  val eth = "ETH"

  implicit val tve = new TokenValueEstimatorImpl()
  tve.setMarketCaps(Map[Address, Double](lrc → 0.8, eth → 1400))
  tve.setTokens(Map[Address, BigInt](lrc → BigInt(1), eth → BigInt(1)))

  val incomeEvaluator = new RingIncomeEstimatorImpl(10)
  val makerExpectFill = ExpectedFill(
    order = Order(id = "maker", tokenS = lrc, tokenB = eth, tokenFee = lrc, walletSplitPercentage = 0.2),
    pending = OrderState(amountS = 100, amountFee = 100),
    amountMargin = 100
  )
  val takerExpectFill = ExpectedFill(
    order = Order(id = "taker", tokenS = eth, tokenB = lrc, tokenFee = lrc, walletSplitPercentage = 0.2),
    pending = OrderState(amountS = 100, amountFee = 100),
    amountMargin = 100
  )

  //info("[sbt core/'testOnly *RingIncomeEvaluatorSpec -- -z incomeEvaluator']")
  "getFiatValue" should "only lrcfee" in {
    //只计算lrcfee时的收益,amountMargin = 0, amountFee = 100
    val ring = Ring(
      makerExpectFill.copy(
        amountMargin = 0,
        pending = OrderState(amountS = 100, amountFee = 100)
      ),
      takerExpectFill.copy(
        amountMargin = 0,
        pending = OrderState(amountS = 100, amountFee = 100)
      )
    )
    val income1 = incomeEvaluator.getIncomeFiatValue(ring)
    //总共收取200lrc，收益应该为0.8*200*(1-0.2)*(1-0.05) = 121.6
    println(income1)
    assert(income1 == 121.6)

  }
  //
  "getFiatValue" should "only margin" in {
    //只计算lrcfee时的收益,amountMargin = 0, amountFee = 100
    val ring = Ring(
      makerExpectFill.copy(
        amountMargin = 100,
        pending = OrderState(amountS = 100, amountFee = 0)
      ),
      takerExpectFill.copy(
        amountMargin = 1,
        pending = OrderState(amountS = 100, amountFee = 0)
      )
    )
    val income1 = incomeEvaluator.getIncomeFiatValue(ring)
    //应该收取1eth,100lrc, 1400*1 + 0.8*100 = 1480
    println(income1)
    assert(income1 == 1480)
  }

  "getFiatValue" should "mix lrc and margin" in {
    //只计算lrcfee时的收益,amountMargin = 0, amountFee = 100
    val ring = Ring(
      makerExpectFill.copy(
        amountMargin = 100,
        pending = OrderState(amountS = 100, amountFee = 100)
      ),
      takerExpectFill.copy(
        amountMargin = 1,
        pending = OrderState(amountS = 100, amountFee = 100)
      )
    )
    val income1 = incomeEvaluator.getIncomeFiatValue(ring)
    //应该收取1eth,100lrc, 1400*1 + 0.8*100 + 0.8*200*(1-0.2)*(1-0.05) = 1601.6
    println(income1)
    assert(income1 == 1601.6)
  }

  "isProfitable" should "test isProfitable" in {
    //只计算lrcfee时的收益,amountMargin = 0, amountFee = 100
    val ring = Ring(
      makerExpectFill.copy(
        amountMargin = 0,
        pending = OrderState(amountS = 100, amountFee = 10)
      ),
      takerExpectFill.copy(
        amountMargin = 0,
        pending = OrderState(amountS = 100, amountFee = 10)
      )
    )
    val income1 = incomeEvaluator.getIncomeFiatValue(ring)
    //应该收取1eth,100lrc, 1400*1 + 0.8*100 + 0.8*200*(1-0.2)*(1-0.05) = 1601.6
    println(income1)
    assert(incomeEvaluator.isProfitable(ring))
  }

}
