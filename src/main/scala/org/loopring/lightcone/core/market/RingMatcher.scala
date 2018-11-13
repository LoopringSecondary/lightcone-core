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

import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.data.MatchingFailure._

import org.slf4s.Logging

trait RingMatcher {
  def matchOrders(
    taker: Order,
    maker: Order,
    minFiatValue: Double
  ): Either[MatchingFailure, OrderRing]
}

//TODO(hongyu): what if an order's tokenB is 0?

class RingMatcherImpl()(
    implicit
    ringIncomeEstimator: RingIncomeEstimator
) extends RingMatcher with Logging {

  def matchOrders(
    taker: Order,
    maker: Order,
    minFiatValue: Double = 0
  ): Either[MatchingFailure, OrderRing] = {
    makeRing(maker, taker) match {
      case Some(ring) if ringIncomeEstimator.isProfitable(ring, minFiatValue) ⇒ Right(ring)
      case Some(ring) ⇒ Left(INCOME_TOO_SMALL)
      case None ⇒ Left(ORDERS_NOT_TRADABLE)
    }
  }

  private def makeRing(maker: Order, taker: Order): Option[OrderRing] = {
    if (maker.amountS * taker.amountS < maker.amountB * taker.amountB ||
      maker.amountB.signum == 0 ||
      taker.amountB.signum == 0 ||
      maker._matchable.isEmpty ||
      taker._matchable.isEmpty) {
      None
    } else {
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
              amountB = Rational(maker.matchable.amountB) * Rational(taker.amountB, taker.amountS)
            )
          )
        } else {
          (
            OrderState(
              amountS = taker.matchable.amountB,
              amountB = Rational(taker.matchable.amountB) * Rational(maker.amountB, maker.amountS)
            ),
            OrderState(
              amountS = taker.matchable.amountS,
              amountB = taker.matchable.amountB
            )
          )
        }

      //fee 按照卖出的比例计算
      val makerFee = maker.matchable.amountFee * makerVolume.amountS / maker.matchable.amountS
      val takerFee = taker.matchable.amountFee * takerVolume.amountS / taker.matchable.amountS

      val makerMargin = (makerVolume.amountS - takerVolume.amountB).max(BigInt(0))
      val takerMargin = (takerVolume.amountS - makerVolume.amountB).max(BigInt(0))

      val ring = OrderRing(
        maker = ExpectedFill(
          order = maker.copy(
            _matchable = Some(OrderState(
              amountS = maker.matchable.amountS - makerVolume.amountS,
              amountB = maker.matchable.amountB - makerVolume.amountB,
              amountFee = maker.matchable.amountFee - makerFee
            ))
          ),
          pending = makerVolume.copy(amountFee = makerFee),
          amountMargin = makerMargin
        ),
        taker = ExpectedFill(
          order = taker.copy(
            _matchable = Some(OrderState(
              amountS = taker.matchable.amountS - takerVolume.amountS,
              amountB = taker.matchable.amountB - takerVolume.amountB,
              amountFee = taker.matchable.amountFee - takerFee
            ))
          ),
          pending = takerVolume.copy(amountFee = takerFee),
          amountMargin = takerMargin
        )
      )

      log.debug(s"makeRing: ${ring}")

      Some(ring)
    }
  }
}
