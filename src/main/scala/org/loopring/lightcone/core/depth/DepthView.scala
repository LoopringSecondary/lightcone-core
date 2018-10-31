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

import scala.collection.mutable

case class DepthEntry(
    price: Double = 0d,
    amountS: Amount = 0
)

// decimal为小数点后精确位数
case class Granularity(
    value: Double = 0.1d,
    decimal: Int = 1
)

class DepthView(
    marketId: MarketId,
    granularity: Granularity,
    maxLength: Int = 1000
)(
    implicit
    orderPool: OrderPool[DepthOrder]
) {

  assert(maxLength > 0)
  // todo: 注意:订单价格使用double表示,极限值:小数点后8位,是否需要改动
  assert(granularity.value >= 0.00000001d)

  // asks是卖出,bids是买入
  private var asks = mutable.SortedMap.empty[Double, DepthEntry]
  private var bids = mutable.SortedMap.empty[Double, DepthEntry]

  def set(order: DepthOrder) = {
    orderPool.getOrder(order.id) match {
      case None ⇒
        orderPool += order
        calculate(order.copy(amountS = 0), order)

      case Some(exist) if exist.amountS != order.amountS ⇒
        orderPool += order
        calculate(exist, order)

      case _ ⇒
    }
  }

  def get() = {
    (asks, bids)
  }

  private[core] def calculate(prev: DepthOrder, next: DepthOrder): Unit = this.synchronized {
    val isAsk = next.isAsk(marketId)
    val orderPrice = next.price.doubleValue()
    val updatedAmount = next.amountS - prev.amountS

    var dest = if (isAsk) asks else bids

    val price = middlePrice(orderPrice)
    val entry = if (isAsk) asks.getOrElse(price, DepthEntry(price, 0)) else bids.getOrElse(price, DepthEntry(price, 0))
    val amount = entry.amountS + updatedAmount
    val newentry = entry.copy(price, amount)

    if (isAsk) asks += price -> newentry else bids += price -> newentry
  }

  private[core] def middlePrice(price: Double): Double = {
    if (price <= granularity.value) {
      granularity.value
    } else {
      ((price / granularity.value).ceil * granularity.value).scaled(granularity.decimal)
    }
  }
}
