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

    val makerSVolume = makerMatchableAmounts.amountS.min(takerMatchableAmounts.amountB)
    val takerSVolume = takerMatchableAmounts.amountS.min(makerMatchableAmounts.amountB)

    val makerMargin = (makerMatchableAmounts.amountS - makerSVolume).min(BigInt(0))
    val takerMargin = (takerMatchableAmounts.amountS - takerSVolume).min(BigInt(0))
    val makerFee = makerMatchableAmounts.amountFee * makerSVolume / makerMatchableAmounts.amountS
    val takerFee = takerMatchableAmounts.amountFee * takerSVolume / takerMatchableAmounts.amountS

    Ring(
      maker = ExpectedFill(
        order = maker,
        pending = OrderState(
          amountS = makerSVolume,
          amountB = takerSVolume,
          amountFee = makerFee
        ),
        amountMargin = makerMargin
      ),
      taker = ExpectedFill(
        order = taker,
        pending = OrderState(
          amountS = takerSVolume,
          amountB = makerSVolume,
          amountFee = takerFee
        ),
        amountMargin = takerMargin
      )
    )
  }
}
