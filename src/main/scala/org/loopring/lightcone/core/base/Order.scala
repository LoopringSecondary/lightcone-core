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

// 注意!!!! 收益不能保证时,合约等比例计算,分母中不包含amountB
case class Order(
    id: ID,
    tokenS: Address,
    tokenB: Address,
    tokenFee: Address,
    amountS: Amount = 0,
    amountB: Amount = 0,
    amountFee: Amount = 0,
    createdAt: Long = -1,
    status: OrderStatus = NEW,
    walletSplitPercentage: Double = 0,
    _outstanding: Option[OrderState] = None,
    _reserved: Option[OrderState] = None,
    _actual: Option[OrderState] = None,
    _matchable: Option[OrderState] = None
) {

  lazy val outstanding = _outstanding.getOrElse(OrderState(amountS, amountB, amountFee))
  lazy val reserved = _reserved.getOrElse(OrderState())
  lazy val actual = _actual.getOrElse(OrderState())
  lazy val matchable = _matchable.getOrElse(OrderState())

  lazy val rate = Rational(amountB, amountS)

  def withOutstandingAmountS(v: Amount) = {
    val r = Rational(v, amountS)
    copy(_outstanding = Some(OrderState(
      (r * Rational(amountS)).bigintValue,
      (r * Rational(amountB)).bigintValue,
      (r * Rational(amountFee)).bigintValue
    )))
  }

  // Advance methods with implicit contextual arguments
  private[core] def requestedAmount()(implicit token: Address): Amount =
    if (token == tokenS && tokenFee == tokenS) {
      outstanding.amountS + outstanding.amountFee
    } else if (token == tokenS && tokenFee != tokenS) {
      outstanding.amountS
    } else if (token != tokenS && tokenFee == tokenB) {
      if (outstanding.amountFee > outstanding.amountB) outstanding.amountFee - outstanding.amountB else 0
    } else {
      outstanding.amountFee
    }

  private[core] def reservedAmount()(implicit token: Address) =
    if (token == tokenS && tokenFee == tokenS) {
      reserved.amountS + reserved.amountFee
    } else if (token == tokenS && tokenFee != tokenS) {
      reserved.amountS
    } else if (token != tokenS && tokenFee == tokenB) {
      reserved.amountB + reserved.amountFee
    } else {
      reserved.amountFee
    }

  // 注意: v < requestAmount
  private[core] def withReservedAmount(v: Amount)(implicit token: Address) =
    if (token == tokenS && tokenFee == tokenS) {
      val r = Rational(amountS, amountFee + amountS)
      val reservedAmountS = (Rational(v) * r).bigintValue()
      copy(_reserved = Some(OrderState(reservedAmountS, 0, v - reservedAmountS))).updateActual()
    } else if (token == tokenS && tokenFee != tokenS) {
      copy(_reserved = Some(OrderState(v, 0, reserved.amountFee))).updateActual()
    } else if (token != tokenS && tokenFee == tokenB) {
      val r = Rational(v, requestedAmount())
      val reservedAmountFee = (Rational(amountFee) * r).bigintValue()
      copy(_reserved = Some(OrderState(reserved.amountS, v - reservedAmountFee, reservedAmountFee))).updateActual()
    } else {
      copy(_reserved = Some(OrderState(reserved.amountS, 0, v))).updateActual()
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

    copy(_actual = Some(OrderState(
      (r * Rational(amountS)).bigintValue,
      (r * Rational(amountB)).bigintValue,
      (r * Rational(amountFee)).bigintValue
    )))
  }

}
