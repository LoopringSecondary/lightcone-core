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
    def sha256(id_ : ID): ByteArray = MessageDigest.getInstance("MD-5")
      .digest(id_.getBytes("UTF-8"))

    sha256(maker.order.id)
      .zip(sha256(taker.order.id))
      .map(p ⇒ p._1 ^ p._2)
      .map(_.toByte)
  }

  def expectedFills() = Seq(maker, taker)
  def orders() = Seq(maker.order, taker.order)
}
