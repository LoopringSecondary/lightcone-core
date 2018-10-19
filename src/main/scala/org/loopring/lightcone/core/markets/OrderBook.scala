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

import org.slf4j.LoggerFactory

case class OrderBookInfo(
    numBuys: Int = 0, // number of visible buy orders
    numSells: Int = 0, // number of visible sell orders
    numHiddenBuys: Int = 0,
    numHiddenSells: Int = 0,
    bestBuyPrice: Option[Rational] = None,
    bestSellPrice: Option[Rational] = None,
    lastPrice: Option[Rational] = None,
    isLastTakerSell: Boolean = false
) {
  def totalNumBuys = numBuys + numHiddenBuys
  def totalNumSells = numSells + numHiddenSells
}

case class MarketId(primaryToken: Address, secondaryToken: Address)

case class OrderBookConfig(
    maxNumBuys: Int,
    maxNumSells: Int,
    maxNumHiddenBuys: Int,
    maxNumHiddenSells: Int
)

trait OrderBook[T] {
  val marketId: MarketId
  val config: OrderBookConfig

  def addOrder(order: Order[T]): Set[Ring[T]]
  def deleteOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]]

  def getLastPrice(): Option[Rational]
  def getOrderBookInfo(): OrderBookInfo

  def getTopBuys(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
  def getTopSells(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
}

abstract class OrderBookImpl[T](
    val marketId: MarketId,
    val config: OrderBookConfig
)(
    implicit
    pendingRingPool: PendingRingPool[T]
)
  extends OrderBook[T] {

  var orderMap = Map.empty[ID, Order[T]]

  private val log = LoggerFactory.getLogger(getClass.getName)

  def addOrder(order: Order[T]): Set[Ring[T]] = {
    order.realActuals
    null
  }
  def deleteOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]] = {
    null
  }

  def getLastPrice(): Option[Rational]
  def getOrderBookInfo(): OrderBookInfo

  def getTopBuys(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
  def getTopSells(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]

  implicit private class RichOrderInMarket[T](order: Order[T]) {
    def isSell = order.tokenS == marketId.secondaryToken
    def isBuy = !isSell
    def price = if (isSell) order.rate else Rational(1) / order.rate

    def realActuals() = {
      val pendingAmountS = pendingRingPool.getOrderPendingAmountS(order.id)
      if (pendingAmountS == 0) order.actuals
      else {
        val actuals = order.actuals
        assert(actuals.amountS > 0)
        val amountS = (actuals.amountS - pendingAmountS).max(0)
        val r = Rational(amountS, actuals.amountS)

        Actuals(
          amountS,
          (Rational(actuals.amountB) * r).bigintValue,
          (Rational(actuals.amountFee) * r).bigintValue,
          actuals.scale * r
        )
      }

    }
  }
}
