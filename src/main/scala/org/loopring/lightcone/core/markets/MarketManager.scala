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

case class OrderBookMetadata(
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

trait MarketManager {

  case class SubmitOrderResult(rings: Seq[Ring], fullyMatchedOrderIds: Seq[ID])
  def submitOrder(order: Order): SubmitOrderResult

  def deleteOrder(order: Order): Boolean

  def triggerMatch(): SubmitOrderResult

  // def getMetadata(): OrderBookMetadata
}
