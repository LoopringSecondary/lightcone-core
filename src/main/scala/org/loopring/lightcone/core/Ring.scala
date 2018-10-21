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

import org.loopring.lightcone.core._
import org.web3j.utils.Numeric
import org.web3j.crypto.{ Hash ⇒ web3Hash }
case class ExpectedFill[T](
    order: Order[T],
    pendingAmountS: Amount = 0,
    pendingAmountB: Amount = 0,
    pendingAmountFee: Amount = 0
) {
  lazy val filledRatio = pendingAmountS ÷ order.amountS
  lazy val outstandingRatio = order.scale - filledRatio
}

case class Ring[T](
    maker: ExpectedFill[T],
    taker: ExpectedFill[T]
) {
  lazy val id: ByteArray = {
    //todo: 需要确定订单结构后才能获取waiveFeePercentage
    val data = Array[Byte]()
      .addHex(maker.order.id)
//      .addUint16(BigInt(maker.order.waiveFeePercentage).bigInteger)
      .addHex(taker.order.id)
//      .addUint16(BigInt(taker.order.waiveFeePercentage).bigInteger)
    web3Hash.sha3(data)
  }

  def expectedFills() = Seq(maker, taker)
  def orders() = Seq(maker.order, taker.order)
}
