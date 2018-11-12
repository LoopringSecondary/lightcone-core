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

package org.loopring.lightcone.core.market

import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.base._

import org.slf4s.Logging

trait PendingRingPool {
  def getOrderPendingAmountS(orderId: String): BigInt
  def hasRing(ringId: String): Boolean

  def addRing(ring: OrderRing): Unit
  def removeRing(ringId: String): Unit
  def removeAllRings(): Unit
  def removeRingsBefore(timestamp: Long): Unit
  def removeRingsOlderThan(age: Long): Unit
  def removeRingsContainingOrder(orderId: String): Unit
}

class PendingRingPoolImpl()(
    implicit
    time: TimeProvider
) extends PendingRingPool with Logging {

  case class OrderInfo(
      pendingAmountS: BigInt = 0,
      ringIds: Set[String] = Set.empty
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
      takerId: String,
      takerPendingAmountS: BigInt,
      makerId: String,
      makerPendingAmountS: BigInt,
      timestamp: Long = time.getCurrentTimeMillis()
  )

  private[core] var orderMap = Map.empty[String, OrderInfo]
  private[core] var ringMap = Map.empty[String, RingInfo]

  def getOrderPendingAmountS(orderId: String): BigInt =
    orderMap.get(orderId).map(_.pendingAmountS).getOrElse(0)

  def hasRing(ringId: String) = ringMap.contains(ringId)

  def addRing(ring: OrderRing) = {
    ringMap.get(ring.id) match {
      case Some(_) ⇒
      case None ⇒
        ringMap += ring.id -> RingInfo(
          ring.taker.id, ring.taker.pending.amountS,
          ring.maker.id, ring.maker.pending.amountS
        )

        incrementPendingAmountS(ring.id, ring.taker.id, ring.taker.pending.amountS)
        incrementPendingAmountS(ring.id, ring.maker.id, ring.maker.pending.amountS)

      // log.debug("pending_orders: " + orderMap.mkString("\n\t"))
    }
  }

  def removeRing(ringId: String) = ringMap.get(ringId) foreach { ringInfo ⇒
    ringMap -= ringId
    decrementPendingAmountS(ringId, ringInfo.takerId, ringInfo.takerPendingAmountS)
    decrementPendingAmountS(ringId, ringInfo.makerId, ringInfo.makerPendingAmountS)
  }

  def removeAllRings() {
    orderMap = Map.empty[String, OrderInfo]
    ringMap = Map.empty[String, RingInfo]
  }

  def removeRingsBefore(timestamp: Long) = ringMap.filter {
    case (_, ringInfo) ⇒ ringInfo.timestamp < timestamp
  }.keys.foreach(removeRing)

  def removeRingsOlderThan(age: Long) =
    removeRingsBefore(time.getCurrentTimeMillis - age)

  def removeRingsContainingOrder(orderId: String) = ringMap.filter {
    case (_, ringInfo) ⇒
      ringInfo.takerId == orderId || ringInfo.makerId == orderId
  }.keys.foreach(removeRing)

  // Private methods
  private def decrementPendingAmountS(ringId: String, orderId: String, pendingAmountS: BigInt) = {
    orderMap.get(orderId) foreach { orderInfo ⇒
      val updated = orderInfo - OrderInfo(pendingAmountS, Set(ringId))
      if (updated.pendingAmountS <= 0) orderMap -= orderId
      else orderMap += orderId -> updated
    }
  }

  def incrementPendingAmountS(ringId: String, orderId: String, pendingAmountS: BigInt) = {
    orderMap += orderId ->
      (orderMap.getOrElse(orderId, OrderInfo()) +
        OrderInfo(pendingAmountS, Set(ringId)))
  }

}
