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

case class Order(
    id: ID,
    tokenS: Address,
    tokenB: Address,
    tokenFee: Option[Address],
    amountS: Amount = 0,
    amountB: Amount = 0,
    amountFee: Amount = 0,
    // original: OrderState,
    createdAt: Long = -1,
    status: OrderStatus = NEW,
    private[core] val _outstanding: Option[OrderState] = None,
    private[core] val _reserved: Option[OrderState] = None,
    private[core] val _actual: Option[OrderState] = None,
    private[core] val _matchable: Option[OrderState] = None
) {

  lazy val outstanding = _outstanding.getOrElse(OrderState(amountS, amountB, amountFee))
  lazy val reserved = _reserved.getOrElse(OrderState())
  lazy val actual = _actual.getOrElse(OrderState())
  lazy val matchable = _matchable.getOrElse(OrderState())

  lazy val rate = Rational(amountB, amountS)

  def withOutstandingAmountS(v: Amount) = {
    var r = Rational(v, amountS)
    copy(
      _outstanding = Some(OrderState(
        (r * Rational(amountS)).bigintValue,
        (r * Rational(amountB)).bigintValue,
        (r * Rational(amountFee)).bigintValue
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

  private[core] def reservedAmount()(implicit token: Address) = {
    val feeIsTokenS = feeSameWithTokenS()

    if ((token == tokenS) && feeIsTokenS) {
      reserved.amountS + reserved.amountFee
    } else if (token == tokenS && !feeIsTokenS) {
      reserved.amountS
    } else {
      reserved.amountFee
    }
  }

  private[core] def withReservedAmount(v: Amount)(implicit token: Address) = {
    val feeIsTokenS = feeSameWithTokenS()

    val state =
      if ((token == tokenS) && feeIsTokenS) {
        val r = Rational(amountS, amountFee + amountS)
        val reservedAmountS = (Rational(v) * r).bigintValue()
        OrderState(reservedAmountS, 0, v - reservedAmountS)
      } else if (token == tokenS && !feeIsTokenS) {
        OrderState(v, 0, reserved.amountFee)
      } else {
        OrderState(reserved.amountS, 0, v)
      }

    copy(_reserved = Some(state)).updateActual()
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
    var r = Rational(reserved.amountS, amountS)
    if (amountFee > 0) {
      r = r min Rational(reserved.amountFee, amountFee)
    }

    copy(
      _actual = Some(OrderState(
        (r * Rational(amountS)).bigintValue,
        (r * Rational(amountB)).bigintValue,
        (r * Rational(amountFee)).bigintValue
      ))
    )
  }

  private def feeSameWithTokenS(): Boolean = tokenFee match {
    case None ⇒ LrcAddress.eq(tokenS)
    case Some(t) if t == tokenS ⇒ true
    case _ ⇒ false
  }

  private def feeSameWithTokenB(): Boolean = tokenFee match {
    case None ⇒ LrcAddress.eq(tokenB)
    case Some(t) if t == tokenB ⇒ true
    case _ ⇒ false
  }
}
