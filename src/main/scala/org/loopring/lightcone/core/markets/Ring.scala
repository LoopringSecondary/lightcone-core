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

import java.security.MessageDigest
import org.web3j.crypto.{ Hash ⇒ web3Hash}
import org.web3j.utils.Numeric

case class ExpectedFill[T](
    order: Order[T],
    pending: Amounts,
    amountMargin: Amount = 0
) {

  def id = order.id
}

case class Ring[T](
    maker: ExpectedFill[T],
    taker: ExpectedFill[T]
) {
  lazy val id: RingID = {
    val data = Array[Byte]()
      .addHex(maker.id)
//      .addUint16(BigInt(maker.waiveFeePercentage).bigInteger)
      .addHex(taker.id)
//      .addUint16(BigInt(maker.waiveFeePercentage).bigInteger)
    web3Hash.sha3(data)
  }

  def expectedFills() = {

    //todo:可用金额，如何获取
    val makerAvailableAmounts = Amounts()
    val takerAvailableAmounts = Amounts()
    val makerSVolume = makerAvailableAmounts.amountS.min(takerAvailableAmounts.amountB)
    val takerSVolume = takerAvailableAmounts.amountS.min(makerAvailableAmounts.amountB)

    val makerMargin = (makerAvailableAmounts.amountS - makerSVolume).min(BigInt(0))
    val takerMargin = (takerAvailableAmounts.amountS - takerSVolume).min(BigInt(0))
    val makerFee = makerAvailableAmounts.amountFee * makerSVolume / makerAvailableAmounts.amountS
    val takerFee = takerAvailableAmounts.amountFee * takerSVolume / takerAvailableAmounts.amountS

    Seq(
      maker.copy(
        pending = Amounts(
          amountS = makerSVolume,
          amountB = takerSVolume,
          amountFee = makerFee
        ),
        amountMargin = makerMargin),
      taker.copy(
        pending = Amounts(
          amountS = takerSVolume,
          amountB = makerSVolume,
          amountFee = takerFee
        ),
        amountMargin = takerMargin)
    )

  }

  def centralRate() = {
    (maker.order.rate + taker.order.rate)/Rational(2)
  }

  def orders() = Seq(maker.order, taker.order)
}
