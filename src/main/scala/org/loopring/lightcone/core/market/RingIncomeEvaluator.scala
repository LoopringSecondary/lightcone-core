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
import org.loopring.lightcone.core.order.TokenMetadataManager

trait RingIncomeEstimator {
  def getIncomeFiatValue(ring: OrderRing): Double
  def isProfitable(ring: OrderRing): Boolean
}

final class RingIncomeEstimatorImpl(
    threshold: Double
)(implicit tve: TokenMetadataManager) extends RingIncomeEstimator {

  def getIncomeFiatValue(ring: OrderRing) =
    getExpectedFillIncomeFiatValue(ring.maker) +
      getExpectedFillIncomeFiatValue(ring.taker)

  def isProfitable(ring: OrderRing) =
    getIncomeFiatValue(ring) >= threshold

  private def getExpectedFillIncomeFiatValue(fill: ExpectedFill)(implicit tve: TokenMetadataManager) = {
    /** 当不包含tokenFee时，无法转换，则返回0
     *  当不包含tokenS时，需要使用tokenB计算
     */
    val (order, pending, amountMargin) = (fill.order, fill.pending, fill.amountMargin)
    val rate = (1 - order.walletSplitPercentage) * (1 - tve.getBurnRate(order.tokenFee))
    val fiatFee = rate * tve.getFiatValue(order.tokenFee, pending.amountFee)
    val fiatMargin = if (tve.hasToken(order.tokenS)) {
      tve.getFiatValue(order.tokenS, amountMargin)
    } else {
      val amountBMargin = Rational(amountMargin * order.amountS, order.amountB).bigintValue()
      tve.getFiatValue(order.tokenB, amountBMargin)
    }
    fiatFee + fiatMargin
  }
}
