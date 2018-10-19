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

case class OrderBookConfig(
    maxNumBuys: Int,
    maxNumSells: Int,
    maxNumHiddenBuys: Int,
    maxNumHiddenSells: Int
)

trait OrderBook[T] {
  def addOrder(order: Order[T]): Set[Ring[T]]
  def deleteOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]]

  def getLastPrice(): Option[Rational]
  def getOrderBookInfo(): OrderBookInfo

  def getTopBuys(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
  def getTopSells(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
}

abstract class OrderBookImpl[T](
    config: OrderBookConfig
)(
    implicit
    pendingRingPool: PendingRingPool[T]
)
  extends OrderBook[T] {

  def addOrder(order: Order[T]): Set[Ring[T]]
  def deleteOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]]

  def getLastPrice(): Option[Rational]
  def getOrderBookInfo(): OrderBookInfo

  def getTopBuys(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
  def getTopSells(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
}
