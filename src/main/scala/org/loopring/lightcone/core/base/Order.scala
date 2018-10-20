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

case class Actuals(
    amountS: Amount = 0,
    amountB: Amount = 0,
    amountFee: Amount = 0,
    scale: Rational = Rational(0)
)

case class Reserved(
    amountS: Amount = 0,
    amountFee: Amount = 0
)

case class Order[T](
    origin: T,
    id: ID,
    tokenS: Address,
    tokenB: Address,
    tokenFee: Option[Address],
    amountS: Amount,
    amountB: Amount,
    amountFee: Amount,
    createdAt: Long = -1,
    status: OrderStatus = NEW,
    reserved: Reserved = Reserved(),
    actuals: Actuals = Actuals()
) {

  lazy val rate = Rational(amountB, amountS)

  // Advance methods with implicit contextual arguments
  private[core] def requestedAmount()(implicit token: Address): Amount = {
    if (token == tokenS) tokenFee match {
      case None ⇒ amountS + amountFee
      case Some(t) if t == tokenS ⇒ amountS + amountFee
      case _ ⇒ amountS
    }
    else if (token == tokenB) tokenFee match {
      case Some(t) if t == tokenB && amountFee > amountB ⇒ amountFee - amountB
      case _ ⇒ 0
    }
    else amountFee
  }

  private[core] def reservedAmount()(implicit token: Address) = tokenFee match {
    case None ⇒ reserved.amountS + reserved.amountFee
    case Some(tokenFee) if token == tokenFee ⇒ reserved.amountFee
    case _ ⇒ reserved.amountS
  }

  private[core] def withReservedAmount(v: Amount)(implicit token: Address) = tokenFee match {
    case None ⇒
      val r = Rational(amountS / (amountFee + amountS))
      val reservedAmountS = (Rational(v) * r).bigintValue
      val reservedAmountFee = v - reservedAmountS

      copy(reserved = Reserved(reservedAmountS, reservedAmountFee))
        .updateActuals()

    case Some(tokenFee) if token == tokenFee ⇒
      copy(reserved = reserved.copy(amountFee = v))
        .updateActuals()

    case _ ⇒
      copy(reserved = reserved.copy(amountS = v))
        .updateActuals()
  }

  // Private methods
  private[core] def as(status: OrderStatus) = {
    assert(status != PENDING)
    copy(
      status = status,
      reserved = Reserved(),
      actuals = Actuals()
    )
  }

  private def updateActuals() = {
    var r = Rational(reserved.amountS, amountS)
    if (amountFee > 0) {
      r = r min Rational(reserved.amountFee, amountFee)
    }

    copy(
      actuals = Actuals(
        (r * Rational(amountS)).bigintValue,
        (r * Rational(amountB)).bigintValue,
        (r * Rational(amountFee)).bigintValue,
        r
      )
    )
  }
}
