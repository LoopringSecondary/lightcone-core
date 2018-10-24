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
import OrderStatus._

case class TokenBalance(
    balance: Amount,
    allowance: Amount,
    availableBalance: Amount,
    availableAllowance: Amount
)

private[core] case class Reservation(
    orderId: ID,
    accumulatedBalance: Amount,
    accumulatedAllowance: Amount
)

class TokenManager(
    val token: Address,
    val maxNumOrders: Int = 1000
)(
    implicit
    orderPool: OrderPool
) extends Object with Logging {
  implicit private val _t = token
  import OrderStatus._

  private[core] var balance: Amount = 0
  private[core] var allowance: Amount = 0
  private[core] var availableBalance: Amount = 0
  private[core] var availableAllowance: Amount = 0

  private[core] var cursor: Int = -1
  private[core] var idxMap = Map.empty[ID, Int]
  private[core] var reservations = Seq.empty[Reservation]
  private[core] val maxSize = 1000

  def size() = reservations.size

  def hasTooManyOrders(): Boolean = size() >= maxNumOrders

  def getTokenBalance() = TokenBalance(
    balance,
    allowance,
    availableBalance,
    availableAllowance
  )

  def init(balance_ : Amount, allowance_ : Amount): Set[ID] = {
    val cursor1 =
      if (balance_ >= balance) cursor
      else {
        val idx = reservations.indexWhere { r ⇒
          r.accumulatedBalance > balance_
        }
        if (idx == -1) cursor else idx - 1
      }

    val cursor2 = if (allowance_ >= allowance) {
      val idx = reservations.indexWhere { r ⇒
        val order = orderPool(r.orderId)
        order.reservedAmount != order.requestedAmount
      }
      if (idx == -1) cursor else idx - 1
    } else {
      val idx = reservations.indexWhere { r ⇒
        r.accumulatedAllowance > allowance_
      }
      if (idx == -1) cursor else idx - 1
    }

    cursor = Math.min(cursor1, cursor2)

    balance = balance_
    allowance = allowance_
    rebalance()
  }

  // returns orders to be deleted
  def reserve(orderId: ID): Set[ID] = {
    assert(orderPool.contains(orderId))
    idxMap.get(orderId) match {
      case Some(_) ⇒ Set.empty
      case None ⇒
        reservations :+= Reservation(orderId, 0, 0)
        rebalance()
    }
  }

  // returns orders to be deleted
  def release(orderId: ID): Set[ID] = {
    idxMap.get(orderId) match {
      case None ⇒ Set.empty
      case Some(idx) ⇒
        reservations = reservations.patch(idx, Nil, 1)
        idxMap -= orderId
        cursor = idx - 1
        rebalance() + orderId
    }
  }

  // returns orders to be deleted
  def adjust(id: ID): Set[ID] = {
    idxMap.get(id) match {
      case None ⇒ Set.empty
      case Some(idx) ⇒
        assert(orderPool.contains(id))
        cursor = Math.min(cursor, idx)
        val order = orderPool(id)
        cursor = idx - 1
        rebalance()
    }
  }

  private[core] def getAccumulatedAtCursor(): (Amount, Amount) = {
    if (cursor < 0) (0, 0)
    else {
      val r = reservations(cursor)
      (r.accumulatedBalance, r.accumulatedAllowance)
    }
  }

  private[core] def rebalance(): Set[ID] = {
    val (goodOnes, badOnes) = reservations.splitAt(cursor + 1)
    reservations = goodOnes

    var (accumulatedBalance, accumulatedAllowance) = getAccumulatedAtCursor()

    availableBalance = balance - accumulatedBalance
    availableAllowance = allowance - accumulatedAllowance

    var ordersToDelete = Set.empty[ID]

    badOnes.foreach { r ⇒
      val order = orderPool(r.orderId)
      val requestedAmount = order.requestedAmount

      if (availableBalance < requestedAmount) {
        ordersToDelete += order.id
        idxMap -= order.id
      } else {
        val reserved =
          if (availableAllowance >= requestedAmount) requestedAmount
          else availableAllowance

        accumulatedBalance += requestedAmount
        accumulatedAllowance += reserved

        availableBalance = balance - accumulatedBalance
        availableAllowance = allowance - accumulatedAllowance

        idxMap += order.id -> reservations.size
        orderPool += order.withReservedAmount(reserved)
        reservations :+= Reservation(
          order.id,
          accumulatedBalance,
          accumulatedAllowance
        )
        cursor += 1
      }
    }

    log.trace("getDebugInfo: " + getDebugInfo)
    log.debug("ordersToDelete: " + ordersToDelete)
    ordersToDelete
  }

  private[core] def getDebugInfo() = {
    val localOrders = reservations.map(r ⇒ orderPool(r.orderId))
    (localOrders, reservations, idxMap, cursor)
  }

}
