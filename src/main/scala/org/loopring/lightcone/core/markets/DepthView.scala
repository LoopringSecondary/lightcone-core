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

import scala.collection.SortedMap

case class DepthDataItem(price: Double, amount: Double, total: Double)
case class DepthData(sells: Seq[DepthDataItem], buys: Seq[DepthDataItem])

private[core] class DepthSide() {
  case class Size(amount: Double, total: Double) {
    def +(another: Size) = Size(
      Math.max(amount + another.amount, 0),
      Math.max(total + another.total, 0)
    )
  }

  private[core] var items = SortedMap.empty[Long, Size]

  def addItem(slot: Long, amount: Double, total: Double) {
    val newSize = items.getOrElse(slot, Size(0, 0)) + Size(amount, total)
    items += slot -> newSize
  }

  def getItems(slotThreshold: Long, numItems: Int): Seq[(Long, Size)] = {
    items.filterKeys(_ > slotThreshold).take(numItems).toSeq
  }
}

private[core] class DepthAggregator(priceDecimals: Int) {
  private val sellDepthSide = new DepthSide
  private val buyDepthSide = new DepthSide

  private val scaling = Math.pow(10, priceDecimals)

  def addItem(isSell: Boolean, price: Double, amount: Double, total: Double) {
    val p =
      if (isSell) Math.ceil(price * scaling).toLong
      else Math.floor(price * scaling).toLong
    if (p > 0) {
      val aggregator = if (isSell) sellDepthSide else buyDepthSide
      aggregator.addItem(p, amount, total)
    }
  }

  def getDepths(middlePrice: Double, numItems: Int): DepthData = {
    val slotThreshold = (middlePrice * scaling).toLong
    val sells = sellDepthSide.getItems(slotThreshold, numItems).map {
      case (p, size) ⇒ DepthDataItem(p / scaling, size.amount, size.total)
    }
    val buys = buyDepthSide.getItems(slotThreshold, numItems).map {
      case (p, size) ⇒ DepthDataItem(p / scaling, size.amount, size.total)
    }

    DepthData(sells, buys)
  }
}

class DepthView(priceDecimals: Int, levels: Int) {
  assert(priceDecimals > levels)

  private val priceDecimalsList = (priceDecimals - levels to priceDecimals)
  private val aggregatorMap = priceDecimalsList.map { scale ⇒
    priceDecimals -> new DepthAggregator(priceDecimals)
  }.toMap

  def getDepths(priceDecimals: Int, middlePrice: Double, numItems: Int): DepthData = {
    aggregatorMap.get(priceDecimals) match {
      case Some(aggregator) ⇒ aggregator.getDepths(middlePrice, numItems)
      case None             ⇒ DepthData(Nil, Nil)
    }
  }

  def addItem(isSell: Boolean, price: Double, amount: Double, total: Double) {
    aggregatorMap.values.foreach(_.addItem(isSell, price, amount, total))
  }
}
