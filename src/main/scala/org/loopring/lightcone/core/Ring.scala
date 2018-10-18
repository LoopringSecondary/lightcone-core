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

case class ExpectedFill[T](
    order: Order[T],
    pendingAmountS: Amount,
    pendingAAmountFee: Amount
) {
  lazy val fillRatio: Double = 0
  lazy val postFillRatio = order.scale - fillRatio
}

case class Ring[T](
    maker: ExpectedFill[T],
    taker: ExpectedFill[T],
    nonce: String,
    createdAt: Timestamp,
    committedAt: Option[Timestamp] = None
) {
  val id: ID = maker.order.id + "-" + taker.order.id // This is not correct
}
