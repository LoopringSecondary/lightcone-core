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

case class OrderState(
    amountS: Amount = 0,
    amountB: Amount = 0,
    amountFee: Amount = 0
)

case class Order[T](
    origin: T,
    id: ID,
    tokenS: Address,
    tokenB: Address,
    tokenFee: Option[Address],
    original: OrderState,
    createdAt: Long = -1,
    status: OrderStatus = NEW,
    private[core] val _outstanding: Option[OrderState] = None,
    private[core] val _reserved: Option[OrderState] = None,
    private[core] val _actual: Option[OrderState] = None,
    private[core] val _matchable: Option[OrderState] = None
) {

  lazy val outstanding = _outstanding.getOrElse(original)
  lazy val reserved = _reserved.getOrElse(OrderState())
  lazy val actual = _actual.getOrElse(OrderState())
  lazy val matchable = _matchable.getOrElse(OrderState())

  lazy val rate = Rational(original.amountB, original.amountS)

  def withOutstandingAmountS(v: Amount) = {
    var r = Rational(v, original.amountS)
    copy(
      _outstanding = Some(OrderState(
        (r * Rational(original.amountS)).bigintValue,
        (r * Rational(original.amountB)).bigintValue,
        (r * Rational(original.amountFee)).bigintValue
      ))
    )
  }

  // Advance methods with implicit contextual arguments
  private[core] def requestedAmount()(implicit token: Address): Amount = {
    if (token == tokenS) tokenFee match {
      case None ⇒ outstanding.amountS + outstanding.amountFee
      case Some(t) if t == tokenS ⇒ outstanding.amountS + outstanding.amountFee
      case _ ⇒ outstanding.amountS
    }
    else if (token == tokenB) tokenFee match {
      case Some(t) if t == tokenB && outstanding.amountFee > outstanding.amountB ⇒
        outstanding.amountFee - outstanding.amountB
      case _ ⇒ 0
    }
    else outstanding.amountFee
  }

  private[core] def reservedAmount()(implicit token: Address) = tokenFee match {
    case None ⇒ reserved.amountS + reserved.amountFee
    case Some(tokenFee) if token == tokenFee ⇒ reserved.amountFee
    case _ ⇒ reserved.amountS
  }

  private[core] def withReservedAmount(v: Amount)(implicit token: Address) =
    tokenFee match {
      case None ⇒
        val r = Rational(original.amountS) / Rational(original.amountFee + original.amountS)
        val reservedAmountS = (Rational(v) * r).bigintValue
        val reservedAmountFee = v - reservedAmountS

        copy(
          _reserved = Some(OrderState(
            reservedAmountS,
            0,
            reservedAmountFee
          ))
        )
          .updateActual()

      case Some(tokenFee) if token == tokenFee ⇒
        copy(_reserved = _reserved.map(_.copy(amountFee = v)))
          .updateActual()

      case _ ⇒
        copy(_reserved = _reserved.map(_.copy(amountS = v)))
          .updateActual()
    }

  // Private methods
  private[core] def as(status: OrderStatus) = {
    assert(status != PENDING)
    copy(
      status = status,
      _reserved = None,
      _actual = None,
      _matchable = None
    )
  }

  private def updateActual() = {
    var r = Rational(reserved.amountS, original.amountS)
    if (original.amountFee > 0) {
      r = r min Rational(reserved.amountFee, original.amountFee)
    }

    copy(
      _actual = Some(OrderState(
        (r * Rational(original.amountS)).bigintValue,
        (r * Rational(original.amountB)).bigintValue,
        (r * Rational(original.amountFee)).bigintValue
      ))
    )
  }
}
