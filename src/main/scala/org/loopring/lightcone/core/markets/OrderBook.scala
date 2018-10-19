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
    numBuys: Int = 0,
    numSells: Int = 0,
    numHiddenBuys: Int = 0,
    numHiddenSells: Int = 0,
    bestBuyPrice: Option[Rational] = None,
    bestSellPrice: Option[Rational] = None,
    lastPrice: Option[Rational] = None,
    isLastTakerSell: Boolean = false
)

trait OrderBook[T] {
  def submitOrder(order: Order[T]): Set[Ring[T]]
  def cancelOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]]

  def getLastPrice(): Option[Rational]
  def getOrderBookInfo(): OrderBookInfo

  def getTopBuys(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
  def getTopSells(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
}

case class OrderBookConfig()

abstract class OrderBookImpl[T](config: OrderBookConfig)
  extends OrderBook[T] {

  def submitOrder(order: Order[T]): Set[Ring[T]]
  def cancelOrder(orderId: ID): Set[RingID]

  def trigerMatch(): Set[Ring[T]]

  def getLastPrice(): Option[Rational]
  def getOrderBookInfo(): OrderBookInfo

  def getTopBuys(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
  def getTopSells(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]]
}