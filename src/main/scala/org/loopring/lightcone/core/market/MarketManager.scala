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

package org.loopring.lightcone.core.market

import org.loopring.lightcone.core.data._

trait MarketManager {
  val marketId: MarketId
  val pendingRingPool: PendingRingPool

  case class MatchResult(
      rings: Seq[OrderRing],
      taker: Order,
      orderbookUpdate: Option[OrderbookUpdate]
  )

  def submitOrder(order: Order, minFiatValue: Double): MatchResult
  def deleteOrder(orderId: String): Option[OrderbookUpdate]
  def deletePendingRing(ringId: String): Option[OrderbookUpdate]
  def getOrder(orderId: String): Option[Order]
  def getMetadata(): MarketMetadata
  def triggerMatch(
    sellOrderAsTaker: Boolean,
    minFiatValue: Double,
    offset: Int = 0
  ): Option[MatchResult]

}
