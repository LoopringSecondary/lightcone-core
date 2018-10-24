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

import org.web3j.crypto.Hash
import org.web3j.utils.Numeric

case class ExpectedFill(
    order: Order,
    pending: OrderState,
    amountMargin: Amount = 0
) {

  def id = order.id

  def getIncomeFiatValue()(implicit tve: TokenValueEstimator) = {
    /** 当不包含tokenFee时，无法转换，则返回0
     *  当不包含tokenS时，需要使用tokenB计算
     */
    val rate = (1 - order.walletSplitPercentage) * (1 - tve.getBurnRate(order.tokenFee))
    val fiatFee = rate * tve.getFiatValue(order.tokenFee, pending.amountFee)
    val fiatMargin = if (tve.canGetMarketCap(order.tokenS)) {
      tve.getFiatValue(order.tokenS, amountMargin)
    } else {
      val amountBMargin = Rational(amountMargin * order.amountS, order.amountB).bigintValue()
      tve.getFiatValue(order.tokenB, amountBMargin)
    }
    fiatFee + fiatMargin
  }
}

case class Ring(
    maker: ExpectedFill,
    taker: ExpectedFill
) {
  // Switching maker and taker should have the same id.
  lazy val id: RingID = {
    Hash.sha3(maker.id)
      .zip(Hash.sha3(taker.id))
      .map(p ⇒ p._1 ^ p._2)
      .map(_.toByte)
      .toArray
  }

  //中间价格，可以在显示深度价格时使用,简单的中间价
  //根据计价token来计算中间价格
  def middleRate(chargeToken: Address): Double = {
    val makerSellPrice = Rational(maker.order.amountS, maker.order.amountB).doubleValue()
    val takerSellPrice = Rational(taker.order.amountS, taker.order.amountB).doubleValue()

    val productPrice = takerSellPrice * makerSellPrice
    val rateOfPrice = math.pow(productPrice, 0.5)
    val priceByMaker = makerSellPrice * rateOfPrice

    if (maker.order.tokenS == chargeToken) priceByMaker
    else 1 / priceByMaker
  }

  def orders() = Seq(maker.order, taker.order)
}
