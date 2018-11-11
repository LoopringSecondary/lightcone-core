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

package org.loopring.lightcone.core.order
import org.loopring.lightcone.core.data._

import org.slf4s.Logging

final private[core] class OrderManagerImpl(
    maxNumOrders: Int
)(
    implicit
    orderPool: OrderPool
) extends OrderManager with Logging {

  assert(maxNumOrders > 0)

  import OrderStatus._

  private[core] implicit var tokens = Map.empty[String, TokenReserveManager]

  def hasTokenReserveManager(token: String): Boolean = {
    tokens.contains(token)
  }
  def addTokenReserveManager(tm: TokenReserveManager) = {
    assert(!hasTokenReserveManager(tm.token))
    tokens += tm.token -> tm
    tm
  }

  def getTokenReserveManager(token: String): TokenReserveManager = {
    assert(hasTokenReserveManager(token))
    tokens(token)
  }

  def submitOrder(order: Order): Boolean = {
    assert(order.amountS > 0)

    assert(tokens.contains(order.tokenS))
    assert(tokens.contains(order.tokenFee))

    if (order.onTokenS(_.hasTooManyOrders) &&
      order.onTokenFee(_.hasTooManyOrders)) {
      orderPool += order.as(CANCELLED_TOO_MANY_ORDERS)
      return false
    }

    orderPool += order.as(NEW)

    if (order.callTokenSAndFeeThenRemoveOrders(_.reserve(order.id))) {
      return false
    }

    orderPool += orderPool(order.id).copy(status = PENDING)
    return true
  }

  def cancelOrder(orderId: String): Boolean = {
    orderPool.getOrder(orderId) match {
      case None ⇒ false
      case Some(order) ⇒
        order.callTokenSAndFeeThenRemoveOrders(_.release(orderId))
        tryRemoveOrder(orderId, CANCELLED_BY_USER)
        true
    }
  }

  // adjust order's outstanding size
  def adjustOrder(orderId: String, outstandingAmountS: BigInt): Boolean = {
    orderPool.getOrder(orderId) match {
      case None ⇒ false
      case Some(order) ⇒
        orderPool += order.withOutstandingAmountS(outstandingAmountS)
        order.callTokenSAndFeeThenRemoveOrders(_.adjust(orderId))
        true
    }
  }

  private type TM = TokenReserveManager
  private type ORDER = Order

  private def tryRemoveOrder(orderId: String, status: OrderStatus) = {
    orderPool.getOrder(orderId) map { order ⇒
      val updated = order.as(status)
      log.debug("delete_by_status: " + updated)
      orderPool += updated
    }
  }

  implicit private class MagicOrder(order: ORDER) {

    def onTokenS[R](method: TM ⇒ R): R =
      method(tokens(order.tokenS))

    def onTokenFee[R](method: TM ⇒ R): R =
      method(tokens(order.tokenFee))

    private def callTokenS_(method: TM ⇒ Map[String, OrderStatus]) =
      onTokenS[Map[String, OrderStatus]](method)

    private def callTokenFee_(method: TM ⇒ Map[String, OrderStatus]) =
      onTokenFee[Map[String, OrderStatus]](method)

    // 删除订单应该有以下几种情况:
    // 1.用户主动删除订单
    // 2.订单成交后变成灰尘单
    // 3.用户账户tokenS balance不足或tokenFee balance不足
    // (除了用户主动操作以外,其他的删除动作都由tokenManager引发)
    // tokenManager的release动作不能由tokenManager本身调用,
    // 只能由orderManager根据并汇总tokenS&tokenFee情况后删除,
    // 删除时tokenS&tokenFee都要删,不能只留一个
    def callTokenSAndFeeThenRemoveOrders(
      method: TM ⇒ Map[String, OrderStatus]
    ): Boolean = {
      (callTokenS_(method) ++ callTokenFee_(method)).map(x ⇒ {
        val id = x._1
        callTokenS_(_.release(id))
        callTokenFee_(_.release(id))
        tryRemoveOrder(id, x._2)
      }).size > 0
    }

  }

}

