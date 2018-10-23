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

import MatchingFailure._

trait RingMatcher {
  def matchOrders(
    taker: Order,
    maker: Order
  ): Either[MatchingFailure, Ring]
}

class SimpleRingMatcher(
    ringIncomeEstimator: RingIncomeEstimator
) extends RingMatcher {

  def matchOrders(
    taker: Order,
    maker: Order
  ): Either[MatchingFailure, Ring] = {
    makeRing(maker, taker) match {
      case Some(ring) if ringIncomeEstimator.isProfitable(ring) ⇒ Right(ring)
      case Some(ring) ⇒ Left(INCOME_TOO_SMALL)
      case None ⇒ Left(ORDERS_NOT_TRADABLE)
    }
  }

  // TODO(hongyu): need to return None if these two order cannot trade with each other
  // because prices don't match.
  private def makeRing(maker: Order, taker: Order): Option[Ring] = {
    if (maker.amountS * taker.amountS < maker.amountB * taker.amountB) {
      return None
    }
    /*合约逻辑：
    取小的成交量计算，按照订单顺序，如果下一单的卖需要缩减，则第一单为最小单
    与顺序相关
    因此生成订单时，按照maker,taker的顺序
     */
    //taker的卖出大于maker的买入时，taker需要缩减，则认为最小交易量为maker的卖出，否则为taker的买入
    val (makerVolume, takerVolume) =
      if (taker.matchable.amountS > maker.matchable.amountB) {
        (
          OrderState(
            amountS = maker.matchable.amountS,
            amountB = maker.matchable.amountB
          ),
          OrderState(
            amountS = maker.matchable.amountB,
            amountB = (Rational(maker.matchable.amountB) *
              Rational(taker.amountB, taker.amountS)).bigintValue
          )
        )
      } else {
        (
          OrderState(
            amountS = taker.matchable.amountB,
            amountB = (Rational(taker.matchable.amountB) *
              Rational(maker.amountB, maker.amountS)).bigintValue
          ),
            OrderState(
              amountS = taker.matchable.amountS,
              amountB = taker.matchable.amountB
            )
        )
      }

    //fee 按照卖出的比例计算
    val makerFee = maker.matchable.amountFee * makerVolume.amountS / maker.amountS
    val takerFee = taker.matchable.amountFee * takerVolume.amountS / taker.amountS

    val makerMargin = (makerVolume.amountS - takerVolume.amountB).max(BigInt(0))
    val takerMargin = (takerVolume.amountS - makerVolume.amountB).max(BigInt(0))

    val ring = Ring(
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

    Some(ring)
  }
}
