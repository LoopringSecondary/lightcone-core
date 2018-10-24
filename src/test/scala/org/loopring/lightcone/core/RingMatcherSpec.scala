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

  val lrc = "LRC"
  val eth = "ETH"

  implicit val tve = new TokenValueEstimatorImpl()
  tve.resetMarketcaps(Map[Address,Double](lrc→0.8, eth→1400))
  tve.resetTokens(Map[Address,BigInt](lrc→BigInt(1), eth→BigInt(1)))

  val incomeEvaluator = new RingIncomeEstimatorImpl(10)
  val simpleMatcher = new SimpleRingMatcher(incomeEvaluator)

  //info("[sbt core/'testOnly *RingMatcherSpec -- -z simpleMatcher']")
  "simpleMatcher" should "case of ORDERS_NOT_TRADABLE" in {
    val maker = Order(
      "maker",
      lrc,
      eth,
      lrc,
      80,
      10,
      5
    )
    val taker = Order(
      "taker",
      eth,
      lrc,
      lrc,
      10,
      100,
      100
    )

    val res = simpleMatcher.matchOrders(taker, maker)
    assert(res.right.toOption.isEmpty && res.left.get == MatchingFailure.ORDERS_NOT_TRADABLE)
  }

  "simpleMatcher" should "case of many decimals price" in {
    val maker = Order(
      "maker",
      lrc,
      eth,
      lrc,
      amountS = BigInt("10000000000"),
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2
    )
    val taker = Order(
      "taker",
      eth,
      lrc,
      lrc,
      10,
      BigInt("10000000000"),
      100
    )

    //放开交易规模，收益足够
    val res = simpleMatcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 10, amountB = BigInt("10000000000"), amountFee = 10))),
      maker.copy(_matchable = Some(OrderState(amountS = BigInt("10000000000"), amountB = 10, amountFee = 10)))
    )
    val testRes = res.right.toOption.nonEmpty && res.left.toOption.isEmpty
    assert(testRes)

  }

  "simpleMatcher" should "case of only fee" in {
    val maker = Order(
      id = "maker",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 100,
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2
    )

    val taker = Order(
      "taker",
      eth,
      lrc,
      lrc,
      10,
      100,
      10,
      walletSplitPercentage = 0.2
    )
    //限制交易规模，收益不足
    val res = simpleMatcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 5, amountB = 50, amountFee = 5))),
      maker.copy(_matchable = Some(OrderState(amountS = 50, amountB = 5, amountFee = 5)))
    )
    val testRes = res.right.toOption.isEmpty && res.left.get == MatchingFailure.INCOME_TOO_SMALL
    info("收益不足的测试结果:" + testRes)
    assert(testRes)

    //放开交易规模，收益足够
    val res1 = simpleMatcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 10))),
      maker.copy(_matchable = Some(OrderState(amountS = 100, amountB = 10, amountFee = 10)))
    )
    val testRes1 = res1.right.toOption.nonEmpty && res1.left.toOption.isEmpty
    info("收益足够时的测试结果:" + testRes1)
    assert(testRes1)
  }

  "simpleMatcher1" should "case of only margin" in {
    val maker = Order(
      id = "maker",
      tokenS = lrc,
      tokenB = eth,
      tokenFee = lrc,
      amountS = 120,
      amountB = 10,
      amountFee = 10,
      walletSplitPercentage = 0.2
    )

    val taker = Order(
      "taker",
      eth,
      lrc,
      lrc,
      10,
      100,
      10,
      walletSplitPercentage = 0.2
    )
    //限制交易规模，收益不足
    val res = simpleMatcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 0))),
      maker.copy(_matchable = Some(OrderState(amountS = 30, amountB = 2, amountFee = 0)))
    )
    val testRes = res.right.toOption.isEmpty && res.left.get == MatchingFailure.INCOME_TOO_SMALL
    info("收益不足的测试结果:" + testRes)
    assert(testRes)

    //放开交易规模，收益足够
    val res1 = simpleMatcher.matchOrders(
      taker.copy(_matchable = Some(OrderState(amountS = 10, amountB = 100, amountFee = 0))),
      maker.copy(_matchable = Some(OrderState(amountS = 120, amountB = 10, amountFee = 0)))
    )
    val testRes1 = res1.right.toOption.nonEmpty && res1.left.toOption.isEmpty
    info("收益足够时的测试结果:" + testRes1)
    assert(testRes1)
  }

}
