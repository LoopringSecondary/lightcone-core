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

trait PendingRingPool {

  def getOrderPendingAmountS(orderId: ID): Amount
  def hasRing(ringId: RingID): Boolean

  def addRing(ring: Ring): Unit
  def removeRing(ringId: RingID): Unit
  def removeAllRings(): Unit
  def removeRingsBefore(timestamp: Long): Unit
  def removeRingsOlderThan(age: Long): Unit
  def removeRingsContainingOrder(orderId: ID): Unit
}

class PendingRingPoolImpl()(
    implicit
    time: TimeProvider
) extends PendingRingPool with Logging {

  case class OrderInfo(
      pendingAmountS: Amount = 0,
      ringIds: Set[RingID] = Set.empty
  ) {

    assert(pendingAmountS >= 0)

    def +(another: OrderInfo) = OrderInfo(
      pendingAmountS + another.pendingAmountS,
      ringIds ++ another.ringIds
    )

    def -(another: OrderInfo) = OrderInfo(
      pendingAmountS - another.pendingAmountS,
      ringIds -- another.ringIds
    )

  }

  case class RingInfo(
      takerId: ID,
      takerPendingAmountS: Amount,
      makerId: ID,
      makerPendingAmountS: Amount,
      timestamp: Long = time.getCurrentTimeMillis()
  )

  private[core] var orderMap = Map.empty[ID, OrderInfo]
  private[core] var ringMap = Map.empty[RingID, RingInfo]

  def getOrderPendingAmountS(orderId: ID): Amount =
    orderMap.get(orderId).map(_.pendingAmountS).getOrElse(0)

  def hasRing(ringId: RingID) = ringMap.contains(ringId)

  def addRing(ring: Ring) = {
    ringMap.get(ring.id) match {
      case Some(_) ⇒
      case None ⇒
        ringMap += ring.id -> RingInfo(
          ring.taker.id, ring.taker.pending.amountS,
          ring.maker.id, ring.maker.pending.amountS
        )

        incrementPendingAmountS(ring.id, ring.taker.id, ring.taker.pending.amountS)
        incrementPendingAmountS(ring.id, ring.maker.id, ring.maker.pending.amountS)

        log.debug("pending_orders: " + orderMap.mkString("\n\t"))
    }
  }

  def removeRing(ringId: RingID) = ringMap.get(ringId) foreach { ringInfo ⇒
    ringMap -= ringId
    decrementPendingAmountS(ringId, ringInfo.takerId, ringInfo.takerPendingAmountS)
    decrementPendingAmountS(ringId, ringInfo.makerId, ringInfo.makerPendingAmountS)
  }

  def removeAllRings() {
    orderMap = Map.empty[ID, OrderInfo]
    ringMap = Map.empty[RingID, RingInfo]
  }

  def removeRingsBefore(timestamp: Long) = ringMap.filter {
    case (_, ringInfo) ⇒ ringInfo.timestamp < timestamp
  }.keys.foreach(removeRing)

  def removeRingsOlderThan(age: Long) =
    removeRingsBefore(time.getCurrentTimeMillis - age)

  def removeRingsContainingOrder(orderId: ID) = ringMap.filter {
    case (_, ringInfo) ⇒
      ringInfo.takerId == orderId || ringInfo.makerId == orderId
  }.keys.foreach(removeRing)

  // Private methods
  private def decrementPendingAmountS(ringId: RingID, orderId: ID, pendingAmountS: Amount) = {
    orderMap.get(orderId) foreach { orderInfo ⇒
      val updated = orderInfo - OrderInfo(pendingAmountS, Set(ringId))
      if (updated.pendingAmountS <= 0) orderMap -= orderId
      else orderMap += orderId -> updated
    }
  }

  def incrementPendingAmountS(ringId: RingID, orderId: ID, pendingAmountS: Amount) = {
    orderMap += orderId ->
      (orderMap.getOrElse(orderId, OrderInfo()) +
        OrderInfo(pendingAmountS, Set(ringId)))
  }

}
