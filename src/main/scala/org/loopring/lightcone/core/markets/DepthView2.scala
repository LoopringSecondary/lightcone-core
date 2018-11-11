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

case object DepthSide2 {
  case class Size(amount: Double, total: Double) {
    def +(another: Size) = Size(
      Math.max(amount + another.amount, 0),
      Math.max(total + another.total, 0)
    )
  }

  def getOrdering(isSell: Boolean) = if (isSell) {
    new Ordering[Long] { def compare(a: Long, b: Long) = a compare b }
  } else {
    new Ordering[Long] { def compare(a: Long, b: Long) = b compare a }
  }
}

private[core] class DepthSide2(
    incrementMode: Boolean,
    isSell: Boolean
) {
  import DepthSide2._

  private implicit val ordering = getOrdering(isSell)
  private[core] var items = SortedMap.empty[Long, Size]
  private[core] var changedItems = Map.empty[Long, Size]

  def addDepthItem(slot: Long, amount: Double, total: Double) {
    if (!incrementMode) {
      throw new UnsupportedOperationException()
    }
    val size = items.getOrElse(slot, Size(0, 0)) + Size(amount, total)
    items += slot -> size
    changedItems += slot -> size
  }

  def setDepthItem(slot: Long, amount: Double, total: Double) {
    if (incrementMode) {
      throw new UnsupportedOperationException()
    }
    val size = Size(amount, total)
    items += slot -> size
  }

  def getItems(slotThreshold: Double, numItems: Int) = {
    val filterKeyFunc =
      if (isSell) (x: Long) ⇒ x >= slotThreshold
      else (x: Long) ⇒ x < slotThreshold

    items.filterKeys(filterKeyFunc)
      .take(numItems)
      .toSeq
  }

  def takeChangedItems() = {
    if (!incrementMode) {
      throw new UnsupportedOperationException()
    }
    val items = changedItems.toSeq
    changedItems = Map.empty
    items
  }

}

private[core] class DepthAggregator2(
    priceDecimals: Int,
    incrementMode: Boolean,
    aggregationLevel: Int
) {
  private val aggregationScaling = Math.pow(10, aggregationLevel)
  private val priceScaling = Math.pow(10, priceDecimals)
  private val sellDepthSide = new DepthSide2(incrementMode, true)
  private val buyDepthSide = new DepthSide2(incrementMode, false)

  def addDepthItem(
    isSell: Boolean,
    price: Long,
    amount: Double,
    total: Double
  ) {
    if (isSell) {
      val p = Math.ceil(price / aggregationScaling).toLong
      sellDepthSide.addDepthItem(p, amount, total)
    } else {
      val p = Math.floor(price / aggregationScaling).toLong
      buyDepthSide.addDepthItem(p, amount, total)
    }
  }

  def setDepthItem(
    isSell: Boolean,
    price: Long,
    amount: Double,
    total: Double
  ) {
    if (isSell) {
      val p = Math.ceil(price / aggregationScaling).toLong
      sellDepthSide.setDepthItem(p, amount, total)
    } else {
      val p = Math.floor(price / aggregationScaling).toLong
      buyDepthSide.setDepthItem(p, amount, total)
    }
  }

  def getDepthData(middlePrice: Double, numItems: Int): DepthData = {
    val slotThreshold = middlePrice * priceScaling / aggregationScaling
    val sells = sellDepthSide.getItems(slotThreshold, numItems).map(toDepthItem)
    val buys = buyDepthSide.getItems(slotThreshold, numItems).map(toDepthItem)
    DepthData(accumulate(sells), accumulate(buys))
  }

  def takeChangedDepthData = DepthData(
    sellDepthSide.takeChangedItems.map(toDepthItem),
    buyDepthSide.takeChangedItems.map(toDepthItem)
  )

  private def toDepthItem(tuple: (Long, DepthSide2.Size)) =
    DepthItem(tuple._1 / priceScaling, tuple._2.amount, tuple._2.total)

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

class DepthView2(priceDecimals: Int, levels: Int) {
  assert(priceDecimals > levels)

  private val levels_ = (0 until levels)
  private val aggregatorMap = levels_.map { level ⇒
    level -> new DepthAggregator2(priceDecimals, false, level)
  }.toMap

  def getDepthData(level: Int, middlePrice: Double, numItems: Int): DepthData = {
    aggregatorMap.get(level) match {
      case Some(aggregator) ⇒ aggregator.getDepthData(middlePrice, numItems)
      case None             ⇒ DepthData(Nil, Nil)
    }
  }

  def setDepthItem(isSell: Boolean, price: Long, amount: Double, total: Double) {
    aggregatorMap.values.foreach(_.addDepthItem(isSell, price, amount, total))
  }
}
