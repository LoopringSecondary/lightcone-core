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

case class DepthItem(price: Double, amount: Double, total: Double)
case class DepthData(sells: Seq[DepthItem], buys: Seq[DepthItem])

private[core] class DepthSide(isSell: Boolean, trackChanges: Boolean) {

  private implicit val ordering = if (isSell) {
    new Ordering[Long] { def compare(a: Long, b: Long) = a compare b }
  } else {
    new Ordering[Long] { def compare(a: Long, b: Long) = b compare a }
  }

  case class Size(amount: Double, total: Double) {
    def +(another: Size) = Size(
      Math.max(amount + another.amount, 0),
      Math.max(total + another.total, 0)
    )
  }

  private[core] var items = SortedMap.empty[Long, Size]
  private[core] var changedItems = Map.empty[Long, Size]

  def addItem(slot: Long, amount: Double, total: Double) {
    val newSize = items.getOrElse(slot, Size(0, 0)) + Size(amount, total)
    items += slot -> newSize
    if (trackChanges) {
      changedItems += slot -> newSize
    }
  }

  def setItem(slot: Long, amount: Double, total: Double) {
    items += slot -> Size(amount, total)
    if (trackChanges) {
      changedItems += slot -> Size(amount, total)
    }
  }

  def getItems(numItems: Int)(filterKeyFunc: Long ⇒ Boolean): Seq[(Long, Size)] = {
    items.filterKeys(filterKeyFunc).take(numItems).toSeq
  }

  def takeChangedItems(): Seq[(Long, Size)] = {
    if (!trackChanges) {
      throw new UnsupportedOperationException()
    }
    val items = changedItems.toSeq
    changedItems = Map.empty
    items
  }
}

private[core] class DepthAggregator(priceDecimals: Int, trackChanges: Boolean) {
  private val sellDepthSide = new DepthSide(true, trackChanges)
  private val buyDepthSide = new DepthSide(false, trackChanges)

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

  def getDepthData(middlePrice: Double, numItems: Int): DepthData = {
    val slotThreshold = (middlePrice * scaling).toLong
    val sells = sellDepthSide.getItems(numItems)(_ >= slotThreshold).map {
      case (p, size) ⇒ DepthItem(p / scaling, size.amount, size.total)
    }
    val buys = buyDepthSide.getItems(numItems)(_ < slotThreshold).map {
      case (p, size) ⇒ DepthItem(p / scaling, size.amount, size.total)
    }

    DepthData(accumulate(sells), accumulate(buys))
  }

  def takeChangedDepthData = {
    val sells = sellDepthSide.takeChangedItems.map {
      case (p, size) ⇒ DepthItem(p / scaling, size.amount, size.total)
    }
    val buys = buyDepthSide.takeChangedItems.map {
      case (p, size) ⇒ DepthItem(p / scaling, size.amount, size.total)
    }

    DepthData(sells, buys)
  }

  private def accumulate(items: Seq[DepthItem]) = {
    var amount: Double = 0
    var total: Double = 0
    items.map { item ⇒
      amount += item.amount
      total += item.total
      DepthItem(item.price, amount, total)
    }
  }
}

class DepthView(priceDecimals: Int, levels: Int) {
  assert(priceDecimals > levels)

  private val priceDecimalsList = (priceDecimals - levels + 1 to priceDecimals)
  private val aggregatorMap = priceDecimalsList.map { priceDecimals ⇒
    priceDecimals -> new DepthAggregator(priceDecimals, false)
  }.toMap

  def getDepthData(priceDecimals: Int, middlePrice: Double, numItems: Int): DepthData = {
    aggregatorMap.get(priceDecimals) match {
      case Some(aggregator) ⇒ aggregator.getDepthData(middlePrice, numItems)
      case None             ⇒ DepthData(Nil, Nil)
    }
  }

  def addItem(isSell: Boolean, price: Double, amount: Double, total: Double) {
    aggregatorMap.values.foreach(_.addItem(isSell, price, amount, total))
  }
}
