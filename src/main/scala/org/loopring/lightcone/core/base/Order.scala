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

import OrderStatus._

case class Order[T](
    origin: T,
    id: ID,
    tokenS: Address,
    tokenB: Address,
    tokenFee: Option[Address],
    amountS: Amount,
    amountB: Amount,
    amountFee: Amount,
    reservedAmountS: Amount = 0,
    reservedAmountFee: Amount = 0,

    status: OrderStatus = NEW,
    actualAmountS: Amount = 0,
    actualAmountB: Amount = 0,
    actualAmountFee: Amount = 0,
    actualScale: Double = 0
) {
  // Advance methods with implicit contextual arguments
  private[core] def requestedAmount()(implicit token: Address) = tokenFee match {
    case None ⇒ amountS + amountFee
    case Some(tokenFee) if token == tokenFee ⇒ amountFee
    case _ ⇒ amountS
  }

  private[core] def reservedAmount()(implicit token: Address) = tokenFee match {
    case None ⇒ reservedAmountS + reservedAmountFee
    case Some(tokenFee) if token == tokenFee ⇒ reservedAmountFee
    case _ ⇒ reservedAmountS
  }

  private[core] def withReservedAmount(v: Amount)(implicit token: Address) = tokenFee match {
    case None ⇒
      val feeRatio = amountFee ÷ (amountFee + amountS)
      val reservedAmountFee_ = v × feeRatio
      val reservedAmountS_ = v - reservedAmountFee_

      withReservedAmountFee(reservedAmountFee_)
        .withReservedAmountS(reservedAmountS_)

    case Some(tokenFee) if token == tokenFee ⇒
      withReservedAmountFee(v)

    case _ ⇒
      withReservedAmountS(v)
  }

  // Private methods
  private[core] def as(status: OrderStatus) = {
    assert(status != PENDING)
    copy(
      reservedAmountS = 0,
      reservedAmountFee = 0,
      status = status,
      actualAmountS = 0,
      actualAmountB = 0,
      actualAmountFee = 0,
      actualScale = 0
    )
  }

  private def withReservedAmountS(v: Amount) =
    copy(reservedAmountS = v).updateActuals()

  private def withReservedAmountFee(v: Amount) =
    copy(reservedAmountFee = v).updateActuals()

  private def updateActuals() = {
    var scale = Rational(reservedAmountS, amountS)
    if (amountFee > 0) {
      scale = scale min Rational(reservedAmountFee, amountFee)
    }

    copy(
      actualAmountS = (scale * Rational(amountS)).bigintValue,
      actualAmountB = (scale * Rational(amountB)).bigintValue,
      actualAmountFee = (scale * Rational(amountFee)).bigintValue,
      actualScale = scale.doubleValue
    )
  }
}
