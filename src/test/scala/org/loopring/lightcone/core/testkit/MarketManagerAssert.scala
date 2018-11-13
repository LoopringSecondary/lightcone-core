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

package org.loopring.lightcone.core.testkit

import org.loopring.lightcone.core.data.Order
import org.loopring.lightcone.core.market._
import org.scalatest._

trait MarketManagerAssert extends Matchers { this: FlatSpec ⇒

  def bidsContainsOrderAssert(order: Order)(implicit marketManager: MarketManagerImpl): Unit = {
    assert(marketManager.bids.contains(order))
  }

  def bidsVolumeAssert(volumeExpect: BigInt, sizeExpect: Int)(implicit marketManager: MarketManagerImpl): Unit = {
    var volume = BigInt(0)
    marketManager.bids.foreach(volume += _.matchable.amountS)
    volume should be(volumeExpect)
    marketManager.bids.size should be(sizeExpect)
  }

  def asksContainsOrderAssert(order: Order)(implicit marketManager: MarketManagerImpl): Unit = {
    assert(marketManager.asks.contains(order))
  }

  def asksVolumeAssert(volumeExpect: BigInt, sizeExpect: Int)(implicit marketManager: MarketManagerImpl): Unit = {
    var volume = BigInt(0)
    marketManager.asks.foreach(volume += _.matchable.amountS)
    volume should be(volumeExpect)
    marketManager.asks.size should be(sizeExpect)
  }

  def pendingAmountAssert(amountSExpect: BigInt, orderId: String)(implicit pendingRingPool: PendingRingPool): Unit = {
    amountSExpect should be(pendingRingPool.getOrderPendingAmountS(orderId))
  }

  def pendingRingAssert(ringId: String)(implicit pendingRingPool: PendingRingPool): Unit = {
    pendingRingPool.hasRing(ringId) should be(true)
  }

}
