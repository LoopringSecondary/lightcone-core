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

import org.web3j.crypto.{Hash ⇒ web3Hash}
import org.web3j.utils.Numeric

case class ExpectedFill(
    order: Order,
    pending: OrderState,
    amountMargin: Amount = 0
) {

  def id = order.id
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

  def expectedFills() = {

    //todo:可用金额，如何获取
    val makerAvailableAmounts = maker.order._matchable match {
      case None ⇒ OrderState()
      case Some(matchable) ⇒ matchable
    }
    val takerAvailableAmounts = maker.order._matchable match {
      case None ⇒ OrderState()
      case Some(matchable) ⇒ matchable
    }

    val makerSVolume = makerAvailableAmounts.amountS.min(takerAvailableAmounts.amountB)
    val takerSVolume = takerAvailableAmounts.amountS.min(makerAvailableAmounts.amountB)

    val makerMargin = (makerAvailableAmounts.amountS - makerSVolume).min(BigInt(0))
    val takerMargin = (takerAvailableAmounts.amountS - takerSVolume).min(BigInt(0))
    val makerFee = makerAvailableAmounts.amountFee * makerSVolume / makerAvailableAmounts.amountS
    val takerFee = takerAvailableAmounts.amountFee * takerSVolume / takerAvailableAmounts.amountS

    Seq(
      maker.copy(
        pending = OrderState(
          amountS = makerSVolume,
          amountB = takerSVolume,
          amountFee = makerFee
        ),
        amountMargin = makerMargin),
      taker.copy(
        pending = OrderState(
          amountS = takerSVolume,
          amountB = makerSVolume,
          amountFee = takerFee
        ),
        amountMargin = takerMargin)
    )

  }

  //中间价格，可以在显示深度价格时使用,简单的中间价
  def centralRate() = {
    (maker.order.rate + taker.order.rate)/Rational(2)
  }

  def orders() = Seq(maker.order, taker.order)
}
