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

class DepthSide() {
  var items = SortedMap.empty[Long, Double]

  def addItem(slot: Long, size: Double) {
    val size_ = items.getOrElse(slot, 0.0) + size
    items += slot -> Math.max(size_, 0)
  }

  def getItems(slotThreshold: Long, numItems: Int): Seq[(Long, Double)] = {
    items.filterKeys(_ > slotThreshold).take(numItems).toSeq
  }
}

case class DepthItem(price: Double, size: Double)
case class Depth(sells: Seq[DepthItem], buys: Seq[DepthItem])

class DepthAggregator(scale: Int) {
  private val sellDepthSide = new DepthSide
  private val buyDepthSide = new DepthSide

  private val factor = Math.pow(10, scale)

  def addItem(isSell: Boolean, price: Double, size: Double) {
    val p = Math.ceil(price * factor).toLong
    if (p > 0) {
      val aggregator = if (isSell) sellDepthSide else buyDepthSide
      aggregator.addItem(p, size)
    }
  }

  def getDepths(middlePrice: Double, numItems: Int): Depth = {
    val slotThreshold = (middlePrice * factor).toLong
    val sells = sellDepthSide.getItems(slotThreshold, numItems).map {
      case (p, s) ⇒ DepthItem(p / factor, s)
    }
    val buys = buyDepthSide.getItems(slotThreshold, numItems).map {
      case (p, s) ⇒ DepthItem(p / factor, s)
    }

    Depth(sells, buys)
  }
}

class DepthView(scale: Int, levels: Int) {
  assert(scale > levels)

  private val scales = (scale - levels to scale)
  private val aggregatorMap = scales.map { scale ⇒
    scale -> new DepthAggregator(scale)
  }.toMap

  def getDepths(scale: Int, middlePrice: Double, numItems: Int): Depth = {
    aggregatorMap.get(scale) match {
      case Some(aggregator) ⇒ aggregator.getDepths(middlePrice, numItems)
      case None             ⇒ Depth(Nil, Nil)
    }
  }

  def addItem(isSell: Boolean, price: Double, size: Double) {
    aggregatorMap.values.foreach(_.addItem(isSell, price, size))
  }
}
