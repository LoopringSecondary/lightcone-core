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

trait PendingRingPool[T] {

  def getOrderPendingAmountS(orderId: ID): Amount

  def addRing(ring: Ring[T]): Unit
  def removeAllRings(): Unit
  def removeRingsBeforeTimestamp(timestamp: Long): Unit
  def removeRingsOlderThan(age: Long): Unit
  def removeRing(ringId: RingID): Unit
  def removeAllRingsWithOrder(orderId: ID): Unit
}

class PendingRingPoolImpl[T]()(
    implicit
    time: TimeProvider
) extends PendingRingPool[T] {

  case class OrderInfo(
      pendingAmountS: Amount = 0,
      ringIds: Set[RingID] = Set.empty
  ) {
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

  def removeAllRings() {
    orderMap = Map.empty[ID, OrderInfo]
    ringMap = Map.empty[RingID, RingInfo]
  }

  def getOrderPendingAmountS(orderId: ID): Amount =
    orderMap.get(orderId).map(_.pendingAmountS).getOrElse(0)

  def addRing(ring: Ring[T]) = {
    ringMap.get(ring.id) match {
      case Some(_) ⇒

      case None ⇒
        ringMap += ring.id -> RingInfo(
          ring.taker.id, ring.taker.pendingAmountS,
          ring.maker.id, ring.maker.pendingAmountS
        )

        orderMap += ring.taker.id ->
          (orderMap.getOrElse(ring.taker.id, OrderInfo()) +
            OrderInfo(ring.taker.pendingAmountS, Set(ring.id)))

        orderMap += ring.maker.id ->
          (orderMap.getOrElse(ring.maker.id, OrderInfo()) +
            OrderInfo(ring.maker.pendingAmountS, Set(ring.id)))
    }
  }

  def removeRingsBeforeTimestamp(timestamp: Long) {
    ringMap.filter {
      case (_, ringInfo) ⇒ ringInfo.timestamp < timestamp
    }.keys.foreach(removeRing)
  }

  def removeRingsOlderThan(age: Long) =
    removeRingsBeforeTimestamp(time.getCurrentTimeMillis - age)

  def removeAllRingsWithOrder(orderId: ID) = {
    ringMap.filter {
      case (_, ringInfo) ⇒
        ringInfo.takerId == orderId || ringInfo.makerId == orderId
    }.keys.foreach(removeRing)
  }

  def removeRing(ringId: RingID) = {
    ringMap.get(ringId) match {
      case None ⇒

      case Some(ringInfo) ⇒
        orderMap.get(ringInfo.takerId) match {
          case None ⇒
          case Some(orderInfo) ⇒
            val updated = orderInfo - OrderInfo(ringInfo.takerPendingAmountS, Set(ringId))
            if (updated.pendingAmountS <= 0) orderMap -= ringInfo.takerId
            else orderMap += ringInfo.takerId -> updated
        }

        orderMap.get(ringInfo.makerId) match {
          case None ⇒
          case Some(orderInfo) ⇒
            val updated = orderInfo - OrderInfo(ringInfo.makerPendingAmountS, Set(ringId))
            if (updated.pendingAmountS <= 0) orderMap -= ringInfo.makerId
            else orderMap += ringInfo.makerId -> updated
        }
    }
  }

}
