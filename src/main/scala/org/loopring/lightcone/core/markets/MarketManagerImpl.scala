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
import scala.collection.mutable.SortedSet

case class MarketManagerConfig(
    maxNumBuys: Int,
    maxNumSells: Int,
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
    config: MarketManagerConfig,
    ringMatcher: RingMatcher
)(
    implicit
    pendingRingPool: PendingRingPool,
    dustOrderEvaluator: DustOrderEvaluator
) extends MarketManager with Logging {

  import MarketManagerImpl._
  import MatchingFailure._

  private implicit val ordering = defaultOrdering()
  private[core] val bids = SortedSet.empty[Order]
  private[core] val asks = SortedSet.empty[Order]

  private val sides = Map(marketId.primary -> bids, marketId.secondary -> asks)

  def submitOrder(order:Order):SubmitOrderResult = {
    deleteOrder(order) //首先删除订单，变为全新订单,否则第二次收到订单，并被匹配之后，可能会一直存在
    val res = submitOrderInternal(order)
    res.affectedOrders.get(order.id) match {
      case None ⇒
        addToSide(order)
      case Some(o) if !dustOrderEvaluator.isDust(o) ⇒
        addToSide(o)
      case _ ⇒ //此时完全匹配，不需要再添加到订单薄
    }
    res
  }

  //在该函数中，taker不会被放入订单簿
  private[core] def submitOrderInternal(order: Order): SubmitOrderResult = {
    log.debug(s"taker order: $order , ${pendingRingPool.getOrderPendingAmountS(order.id)} ")

    var rings = Seq.empty[Ring]
    var makerOrdersRecyclable = Seq.empty[Order]
    var fullyMatchedOrderIds = Seq.empty[ID]
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

    println("####", taker)
    if (dustOrderEvaluator.isDust(taker)) {
      println("isDust",taker)
      fullyMatchedOrderIds :+= taker.id
      affectedOrders += taker.id → taker.copy(_matchable = Some(OrderState()))
      return SubmitOrderResult(rings, fullyMatchedOrderIds, affectedOrders)
    }

    @tailrec
    def recursivelyMatchOrder(): Unit = {
      val matchResult = for {
        maker ← takeTopMaker(taker)
      } yield (maker, ringMatcher.matchOrders(taker, maker))

      matchResult match {
        case Some((maker, Left(failure))) ⇒ failure match {
          case ORDERS_NOT_TRADABLE ⇒
            log.debug(s"match failed:$ORDERS_NOT_TRADABLE --taker:$taker, maker:$maker")
            makerOrdersRecyclable :+= maker

          case INCOME_TOO_SMALL ⇒
            log.debug(s"match failed:$INCOME_TOO_SMALL --taker:$taker, maker:$maker")
            makerOrdersRecyclable :+= maker
            recursivelyMatchOrder()
        }

        case Some((maker, Right(ring))) ⇒
          rings :+= ring

          taker = ring.taker.order
          val updatedMaker = ring.maker.order

          if (dustOrderEvaluator.isDust(updatedMaker)) {
            affectedOrders += updatedMaker.id → updatedMaker.copy(_matchable = Some(OrderState()))
            fullyMatchedOrderIds :+= updatedMaker.id
          } else {
            affectedOrders += updatedMaker.id → updatedMaker
            makerOrdersRecyclable :+= updatedMaker
          }

          if (dustOrderEvaluator.isDust(taker)) {
            affectedOrders += taker.id → taker.copy(_matchable = Some(OrderState()))
            fullyMatchedOrderIds :+= taker.id
          } else {
            recursivelyMatchOrder()
          }

        case None ⇒
          if (!dustOrderEvaluator.isDust(taker)) {
            affectedOrders += taker.id → taker
          } else {
            affectedOrders += taker.id → taker.copy(_matchable = Some(OrderState()))
          }
      }
    }

    recursivelyMatchOrder()

    makerOrdersRecyclable.foreach(addToSide)

    rings.foreach(pendingRingPool.addRing)

    SubmitOrderResult(rings, fullyMatchedOrderIds, affectedOrders)
  }

  def deleteOrder(order: Order): Boolean = {
    sides(order.tokenS).remove(order) //pending应该不用清除，而是等待以太坊事件回调或过期
  }

  def deletePendingRing(ring: Ring): Unit = {
    pendingRingPool.removeRing(ring.id)
  }

  def triggerMatch(): SubmitOrderResult = {
    val maxBidsPrice = (bids.headOption map {
      head ⇒ Rational(head.amountS, head.amountB)
    }).getOrElse(Rational(0))
    val rationalOne = Rational(1)

    var rings = Seq.empty[Ring]
    var fullyMatchedOrderIds = Set.empty[ID]
    var affectedOrders = Map.empty[ID, Order]
    var askOrdersRecyclable = Seq.empty[Order]

    @tailrec
    def recursivelyReMatch(): Unit = {
      popOrder(asks) match {
        case Some(taker)  ⇒
          log.debug(s"triggerMatch --- ask:${taker.id}")
          //submitOrder会在在taker最后不为灰尘单时，会重新放入首部，
          //因此需要保证不会taker再放入asks
          val submitRes = submitOrderInternal(taker)
          submitRes.affectedOrders.get(taker.id) match {
            case None ⇒
              askOrdersRecyclable :+= taker
            case Some(o) if !dustOrderEvaluator.isDust(o) ⇒
              askOrdersRecyclable :+= o
            case _ ⇒
          }
          rings ++= submitRes.rings
          fullyMatchedOrderIds ++= submitRes.fullyMatchedOrderIds
          affectedOrders ++= submitRes.affectedOrders
          if (Rational(taker.amountS, taker.amountB) * maxBidsPrice >= rationalOne) {
            recursivelyReMatch()
          }
        case _ ⇒
      }
    }

    recursivelyReMatch()

    askOrdersRecyclable.foreach(asks.add)

    SubmitOrderResult(rings, fullyMatchedOrderIds.toSeq, affectedOrders)
  }

  private def addToSide(order: Order) = {
    val side = sides(order.tokenS)
    //添加到side
    side.add(order)
    //是否需要删除
    val maxNum = if (marketId.primary == order.tokenS) config.maxNumBuys else config.maxNumSells
    if (maxNum > 0 && maxNum < side.size) {
      side.lastOption map {
        last ⇒
          log.debug(s"due to too many orders,order:${last.id} will be removed.")
          side.remove(last)
      }
    }
  }

  private def takeTopMaker(order: Order): Option[Order] = {
    popOrder(sides(order.tokenB))
  }

  private def popOrder(side: SortedSet[Order]) = {
    side.headOption.map {
      head ⇒
        side.remove(head)
        head
    }
  }

}
