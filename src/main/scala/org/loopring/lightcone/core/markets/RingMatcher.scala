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

trait RingMatcher {
  def matchOrders(
    taker: Order,
    maker: Order
  ): Either[MatchingFailure.Value, Ring]
}

class SimpleRingMatcher(
    ringIncomeEstimator: RingIncomeEstimator
) extends RingMatcher {

  def matchOrders(
    taker: Order,
    maker: Order
  ): Either[MatchingFailure.Value, Ring] = {
    val ring = createRing(maker, taker)
    if (ringIncomeEstimator.isProfitable(ring)) {
      Right(ring)
    } else {
      Left(MatchingFailure.INCOME_NOT_ENOUGH)
    }
  }

  def createRing(maker: Order, taker: Order): Ring = {

    val makerMatchableAmounts = maker._matchable match {
      case None            ⇒ OrderState()
      case Some(matchable) ⇒ matchable
    }
    val takerMatchableAmounts = maker._matchable match {
      case None            ⇒ OrderState()
      case Some(matchable) ⇒ matchable
    }

    /*合约逻辑：
    取小的成交量计算，按照订单顺序，如果下一单的卖需要缩减，则第一单为最小单
    与顺序相关
    因此生成订单时，按照maker,taker的顺序
     */
    //taker的卖出大于maker的买入时，taker需要缩减，则认为最小交易量为maker的卖出，否则为taker的买入
    val (makerVolume, takerVolume) = if (takerMatchableAmounts.amountS > makerMatchableAmounts.amountB) {
      val takerSVolume = makerMatchableAmounts.amountB
      val takerBVolume = (Rational(takerSVolume) * Rational(taker.amountB, taker.amountS)).bigintValue()

      val makerBVolume = makerMatchableAmounts.amountB
      val makerSVolume = makerMatchableAmounts.amountS

      (
        OrderState(
          amountS = makerSVolume,
          amountB = makerBVolume
        ),
        OrderState(
          amountS = takerSVolume,
          amountB = takerBVolume
        )
      )
    } else {
      val takerSVolume = takerMatchableAmounts.amountS
      val takerBVolume = takerMatchableAmounts.amountB

      val makerSVolume = takerMatchableAmounts.amountB
      val makerBVolume = (Rational(makerSVolume) * Rational(maker.amountB, maker.amountS)).bigintValue()

      (
        OrderState(
          amountS = makerSVolume,
          amountB = makerBVolume
        ),
        OrderState(
          amountS = takerSVolume,
          amountB = takerBVolume
        )
      )
    }

    val makerMargin = (makerVolume.amountS - takerVolume.amountB).min(BigInt(0))
    val takerMargin = (takerVolume.amountS - makerVolume.amountB).min(BigInt(0))
    //fee 按照卖出的比例计算
    val makerFee = makerMatchableAmounts.amountFee * makerVolume.amountS / makerMatchableAmounts.amountS
    val takerFee = takerMatchableAmounts.amountFee * takerVolume.amountS / takerMatchableAmounts.amountS

    Ring(
      maker = ExpectedFill(
        order = maker,
        pending = makerVolume.copy(amountFee = makerFee),
        amountMargin = makerMargin
      ),
      taker = ExpectedFill(
        order = taker,
        pending = takerVolume.copy(amountFee = takerFee),
        amountMargin = takerMargin
      )
    )
  }
}
