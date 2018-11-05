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
import scala.annotation.tailrec
import scala.collection.mutable.{ SortedSet, Map ⇒ MMap }

case class MarketManagerConfig(
    maxNumbersOfOrders: Int // TODO(daniel): this is not supported yet.
)

object MarketManagerImpl {
  private def defaultOrdering() = new Ordering[Order] {
    def compare(a: Order, b: Order) = {
      if (a.rate < b.rate) -1
      else if (a.rate > b.rate) 1
      else if (a.createdAt < b.createdAt) -1
      else if (a.createdAt > b.createdAt) 1
      else a.id compare b.id //在rate和createAt相同时，根据id排序，否则会丢单
    }
  }
}

class MarketManagerImpl(
    val marketId: MarketId,
    val config: MarketManagerConfig,
    val ringMatcher: RingMatcher
)(
    implicit
    pendingRingPool: PendingRingPool,
    dustOrderEvaluator: DustOrderEvaluator
) extends MarketManager with Logging {

  import MarketManagerImpl._
  import MatchingFailure._
  import OrderStatus._

  private implicit val ordering = defaultOrdering()
  private[core] val primaries = SortedSet.empty[Order] // order.tokenS == marketId.primary
  private[core] val secondaries = SortedSet.empty[Order] // order.tokenS == marketId.secondary
  private[core] val orderMap = MMap.empty[ID, Order]

  private[core] val sides = Map(
    marketId.primary -> primaries,
    marketId.secondary -> secondaries
  )

  def submitOrder(order: Order): SubmitOrderResult = {
    // Allow re-submission of an existing order. In such case, we need to remove the original
    // copy of the order first.
    removeOrderFromSide(order)

    val res = matchOrders(order)

    res.affectedOrders.get(order.id) match {
      case None ⇒
        addOrderToSide(order) // why none???
      case Some(o) if !dustOrderEvaluator.isDust(o) ⇒
        addOrderToSide(o)
      case _ ⇒ //此时完全匹配，不需要再添加到订单薄
    }
    res
  }

  // Recursively match the taker with makers. The taker order will NOT be added to its side
  // by this method.
  private[core] def matchOrders(order: Order): SubmitOrderResult = {
    log.debug(s"taker order: $order , ${pendingRingPool.getOrderPendingAmountS(order.id)} ")

    var rings = Set.empty[OrderRing]
    var makersToAddBack = Set.empty[Order]
    var fullyMatchedOrders = Set.empty[Order]
    var affectedOrders = Map.empty[ID, Order]

    val subedPendingAmountS =
      order.actual.amountS -
        pendingRingPool.getOrderPendingAmountS(order.id)

    var taker = order.copy(_matchable = Some(OrderState(
      amountS = subedPendingAmountS,
      amountB = Rational(
        subedPendingAmountS * order.amountB,
        order.amountS
      ).bigintValue(),
      amountFee = Rational(
        subedPendingAmountS * order.amountFee,
        order.amountS
      ).bigintValue()
    )))

    if (dustOrderEvaluator.isDust(taker)) {
      fullyMatchedOrders += taker
      affectedOrders += taker.id → taker.copy(_matchable = Some(OrderState()))

      return SubmitOrderResult(rings, fullyMatchedOrders, affectedOrders, Some(taker))
    }

    @tailrec
    def recursivelyMatchOrders(): Unit = {
      val matchResult = popTopMakerOrder(taker).map { maker ⇒
        (maker, ringMatcher.matchOrders(taker, maker))
      }

      matchResult match {
        case Some((maker, Left(failure))) ⇒
          log.debug(s"match failure $failure, taker: $taker, maker: $maker")
          makersToAddBack += maker

          if (failure == INCOME_TOO_SMALL) {
            recursivelyMatchOrders()
          }

        case Some((maker, Right(ring))) ⇒
          val _taker = taker
          taker = ring.taker.order // update the taker order
          val updatedMaker = ring.maker.order
          log.debug(s"""
            match success.
            taker: ${_taker}
            maker: $maker
            updatedTaker: $taker
            updatedMaker: $updatedMaker
            ring: $ring """)

          rings += ring

          if (dustOrderEvaluator.isMatchableDust(updatedMaker)) {
            affectedOrders += updatedMaker.id → updatedMaker.copy(_matchable = Some(OrderState()))
            fullyMatchedOrders += updatedMaker
          } else {
            affectedOrders += updatedMaker.id → updatedMaker
            makersToAddBack += updatedMaker
          }

          if (dustOrderEvaluator.isMatchableDust(taker)) {
            affectedOrders += taker.id → taker.copy(_matchable = Some(OrderState()))
            fullyMatchedOrders += taker
          } else {
            recursivelyMatchOrders()
          }

        case None ⇒
          if (dustOrderEvaluator.isMatchableDust(taker)) {
            taker = taker.copy(status = COMPLETELY_FILLED)
          }
      }
    }

    recursivelyMatchOrders()

    makersToAddBack.foreach(addOrderToSide)

    rings.foreach(pendingRingPool.addRing)

    SubmitOrderResult(rings, fullyMatchedOrders, affectedOrders, Some(taker))
  }

  def triggerMatch(): SubmitOrderResult = {
    val maxBidsPrice = (primaries.headOption map {
      head ⇒ Rational(head.amountS, head.amountB)
    }).getOrElse(Rational(0))
    val rationalOne = Rational(1)

    var rings = Set.empty[OrderRing]
    var makersToAddBack = Set.empty[Order]
    var fullyMatchedOrders = Set.empty[Order]
    var affectedOrders = Map.empty[ID, Order]

    @tailrec
    def recursivelyReMatch(): Unit = {
      popOrder(secondaries) match {
        case Some(taker) ⇒
          log.debug(s"triggerMatch --- ask:${taker.id}")
          //submitOrder会在在taker最后不为灰尘单时，会重新放入首部，
          //因此需要保证不会taker再放入secondaries
          val submitRes = matchOrders(taker)
          submitRes.affectedOrders.get(taker.id) match {
            case None ⇒
              makersToAddBack += taker
            case Some(o) if !dustOrderEvaluator.isDust(o) ⇒
              makersToAddBack += o
            case _ ⇒
          }
          rings ++= submitRes.rings
          fullyMatchedOrders ++= submitRes.fullyMatchedOrders
          affectedOrders ++= submitRes.affectedOrders
          if (Rational(taker.amountS, taker.amountB) * maxBidsPrice >= rationalOne) {
            recursivelyReMatch()
          }
        case _ ⇒
      }
    }

    recursivelyReMatch()

    makersToAddBack.foreach(secondaries.add)

    SubmitOrderResult(rings, fullyMatchedOrders, affectedOrders, None)
  }

  def deleteOrder(orderId: ID): Boolean = {
    orderMap.get(orderId) match {
      case None ⇒ false
      case Some(order) ⇒
        removeOrderFromSide(order)
        true
    }
  }

  def deletePendingRing(ring: OrderRing): Unit = {
    pendingRingPool.removeRing(ring.id)
  }

  // Add an order to its side.
  private def addOrderToSide(order: Order) = {
    orderMap += order.id -> order
    sides(order.tokenS) += order
  }

  private def removeOrderFromSide(order: Order) = {
    orderMap -= order.id
    sides(order.tokenS) -= order
  }

  // Remove and return the top taker order for a taker order.
  private def popTopMakerOrder(order: Order): Option[Order] =
    popOrder(sides(order.tokenB))

  // Remove and return the top order from one side.
  private def popOrder(side: SortedSet[Order]): Option[Order] = {
    side.headOption.map { order ⇒
      orderMap -= order.id
      side -= order
      order
    }
  }
}
