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

import org.web3j.crypto.{ Hash ⇒ web3Hash }
import org.web3j.utils.Numeric

import scala.{ math ⇒ mathInS }

case class ExpectedFill(
    order: Order,
    pending: OrderState,
    amountMargin: Amount = 0
) {

  def id = order.id

  def getFiatValue()(implicit tve: TokenValueEstimator) = {
    val tokenFee = order.tokenFee.getOrElse(order.tokenS)
    val rate = (1 - this.order.walletSplitPercentage) * (1 - tve.getBurnRate(tokenFee))
    rate * tve.getFiatValue(
      tokenFee,
      this.pending.amountFee
    ) +
      tve.getFiatValue(
        order.tokenS,
        this.amountMargin
      )
  }
}

case class Ring(
    maker: ExpectedFill,
    taker: ExpectedFill
) {
  lazy val id: RingID = {
    val data = Numeric.hexStringToByteArray(maker.id) ++
      Numeric.hexStringToByteArray(taker.id)
    web3Hash.sha3(data)
  }

  //中间价格，可以在显示深度价格时使用,简单的中间价
  //根据计价token来计算中间价格
  def centralRate(chargeToken: Address): Double = {
    val makerSellPrice = Rational(maker.order.amountS, maker.order.amountB).doubleValue()
    val takerSellPrice = Rational(taker.order.amountS, taker.order.amountB).doubleValue()
    val productPrice = takerSellPrice * makerSellPrice
    val rateOfPrice = mathInS.pow(productPrice, 0.5)
    val priceByMaker = makerSellPrice * rateOfPrice
    if (maker.order.amountS equals chargeToken) priceByMaker else 1 / priceByMaker
  }

  def orders() = Seq(maker.order, taker.order)
}
