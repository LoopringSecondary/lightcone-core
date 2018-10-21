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
import org.slf4s.Logging

final private[core] class OrderStateManagerImpl[T](
    maxNumOrders: Int
)(
    implicit
    orderPool: OrderPool[T]
) extends OrderStateManager[T] with Logging {

  assert(maxNumOrders > 0)

  import OrderStatus._

  private[core] implicit var tokens = Map.empty[Address, TokenManager[T]]

  def hasTokenManager(token: Address): Boolean = {
    tokens.contains(token)
  }
  def addTokenManager(tm: TokenManager[T]) = {
    assert(!hasTokenManager(tm.token))
    tokens += tm.token -> tm
    tm
  }

  def getTokenManager(token: Address): TokenManager[T] = {
    assert(hasTokenManager(token))
    tokens(token)
  }

  def submitOrder(order: Order[T]): Boolean = {
    assert(order.original.amountS > 0)

    assert(tokens.contains(order.tokenS))
    if (order.tokenFee.nonEmpty) {
      assert(tokens.contains(order.tokenFee.get))
    }

    if (order.onTokenS(_.hasTooManyOrders) &&
      order.onTokenFee(_.hasTooManyOrders).getOrElse(false)) {
      orderPool += order.as(CANCELLED_TOO_MANY_ORDERS)
      return false
    }

    orderPool += order.as(NEW)

    if (order.callTokenSThenRemoveOrders(_.reserve(order.id)) ||
      order.callTokenFeeThenRemoveOrders(_.reserve(order.id))) {
      return false
    }

    orderPool += orderPool(order.id).copy(status = PENDING)
    return true
  }

  def cancelOrder(orderId: ID): Boolean = {
    orderPool.getOrder(orderId) match {
      case None ⇒ false
      case Some(order) ⇒
        order.callTokenSThenRemoveOrders(_.release(orderId), CANCELLED_BY_USER)
        order.callTokenFeeThenRemoveOrders(_.release(orderId), CANCELLED_BY_USER)
        tryRemoveOrder(orderId, CANCELLED_BY_USER)
        true
    }
  }

  // adjust order's outstanding size
  def adjustOrder(orderId: ID, outstandingAmountS: Amount): Boolean = {
    orderPool.getOrder(orderId) match {
      case None ⇒ false
      case Some(order) ⇒
        orderPool += order.withOutstandingAmountS(outstandingAmountS)
        order.callTokenSThenRemoveOrders(_.adjust(orderId))
        order.callTokenFeeThenRemoveOrders(_.adjust(orderId))
        true
    }
  }

  private type TM = TokenManager[T]
  private type ORDER = Order[T]

  private def tryRemoveOrder(orderId: ID, status: OrderStatus) = {
    orderPool.getOrder(orderId) map { order ⇒
      val updated = order.as(status)
      log.debug("delete_by_status: " + updated)
      orderPool += updated
    }
  }

  implicit private class MagicOrder[T](order: ORDER) {

    def onTokenS[R](method: TM ⇒ R): R =
      method(tokens(order.tokenS))

    def onTokenFee[R](method: TM ⇒ R): Option[R] =
      order.tokenFee.map(tokens(_)).map(method)

    def callTokenSThenRemoveOrders(
      method: TM ⇒ Set[ID],
      status: OrderStatus = CANCELLED_LOW_BALANCE
    ): Boolean = {
      val deleted = callTokenS_(method).map { id ⇒
        callTokenS_(_.release(id))
        tryRemoveOrder(id, status)
      }

      deleted.size > 0
    }

    def callTokenFeeThenRemoveOrders(
      method: TM ⇒ Set[ID],
      status: OrderStatus = CANCELLED_LOW_FEE_BALANCE
    ): Boolean = {
      val deleted = callTokenFee_(method).map { id ⇒
        callTokenFee_(_.release(id))
        tryRemoveOrder(id, status)
      }

      deleted.size > 0
    }

    private def callTokenS_(method: TM ⇒ Set[ID]) =
      onTokenS[Set[ID]](method)

    private def callTokenFee_(method: TM ⇒ Set[ID]) =
      onTokenFee[Set[ID]](method).getOrElse(Set.empty)

  }

}

