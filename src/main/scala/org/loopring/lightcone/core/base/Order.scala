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

object AskTokenMatchType extends Enumeration {
  type AskTokenMatchType = Value

  val ASK_TOKEN_S_AND_FEE = Value // 查询tokenS, tokenS == tokenFee
  val ASK_TOKEN_B_AND_FEE = Value // 查询tokenFee, tokenFee == tokenB
  val ASK_TOKEN_S_ONLY = Value // 查询tokenS, tokenS != tokenFee
  val ASK_TOKEN_FEE_ONLY = Value // 查询tokenFee, tokenFee != tokenS && tokenFee != tokenB
}

// 注意!!!! 收益不能保证时,合约等比例计算,分母中不包含amountB
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
  private[core] def requestedAmount()(implicit token: Address): Amount = askMatchType() match {
    case AskTokenMatchType.ASK_TOKEN_S_AND_FEE ⇒
      outstanding.amountS + outstanding.amountFee

    case AskTokenMatchType.ASK_TOKEN_B_AND_FEE ⇒
      if (outstanding.amountFee > outstanding.amountB) outstanding.amountFee - outstanding.amountB else 0

    case AskTokenMatchType.ASK_TOKEN_S_ONLY ⇒
      outstanding.amountS

    case AskTokenMatchType.ASK_TOKEN_FEE_ONLY ⇒
      outstanding.amountFee
  }

  private[core] def reservedAmount()(implicit token: Address) = askMatchType() match {
    case AskTokenMatchType.ASK_TOKEN_S_AND_FEE ⇒
      reserved.amountS + reserved.amountFee

    case AskTokenMatchType.ASK_TOKEN_B_AND_FEE ⇒
      reserved.amountB + reserved.amountFee

    case AskTokenMatchType.ASK_TOKEN_S_ONLY ⇒
      reserved.amountS

    case AskTokenMatchType.ASK_TOKEN_FEE_ONLY ⇒
      reserved.amountFee
  }

  // 注意: v < requestAmount
  private[core] def withReservedAmount(v: Amount)(implicit token: Address) = askMatchType() match {
    case AskTokenMatchType.ASK_TOKEN_S_AND_FEE ⇒
      val r = Rational(amountS, amountFee + amountS)
      val reservedAmountS = (Rational(v) * r).bigintValue()
      copy(_reserved = Some(OrderState(reservedAmountS, 0, v - reservedAmountS))).updateActual()

    case AskTokenMatchType.ASK_TOKEN_B_AND_FEE ⇒
      val r = Rational(v, requestedAmount())
      val reservedAmountFee = (Rational(amountFee) * r).bigintValue()
      copy(_reserved = Some(OrderState(reserved.amountS, v - reservedAmountFee, reservedAmountFee))).updateActual()

    case AskTokenMatchType.ASK_TOKEN_S_ONLY ⇒
      copy(_reserved = Some(OrderState(v, 0, reserved.amountFee))).updateActual()

    case AskTokenMatchType.ASK_TOKEN_FEE_ONLY ⇒
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

  private[core] def nonEmptyTokenFee: Address = {
    tokenFee match {
      case None    ⇒ LrcAddress.lrc
      case Some(t) ⇒ t
    }
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

  private def askMatchType()(implicit token: Address): AskTokenMatchType.Value = {
    val (feeIsTokenS, feeIsTokenB) = tokenFee match {
      case None ⇒ (LrcAddress.eq(tokenS), false)
      case Some(t) if t == tokenS ⇒ (true, false)
      case Some(t) if t == tokenB ⇒ (false, true)
      case _ ⇒ (false, false)
    }

    if (token == tokenS && feeIsTokenS) {
      AskTokenMatchType.ASK_TOKEN_S_AND_FEE
    } else if (token == tokenS && !feeIsTokenS) {
      AskTokenMatchType.ASK_TOKEN_S_ONLY
    } else if (token != tokenS && feeIsTokenB) {
      AskTokenMatchType.ASK_TOKEN_B_AND_FEE
    } else if (token != tokenS && !feeIsTokenB) {
      AskTokenMatchType.ASK_TOKEN_FEE_ONLY
    } else {
      throw new Exception("ask token:" + token.toString + " invalid")
    }
  }
}
