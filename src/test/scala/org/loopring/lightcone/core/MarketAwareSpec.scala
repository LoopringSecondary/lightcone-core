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

import org.loopring.lightcone.core.base._
import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.market._

trait MarketAwareSpec extends OrderAwareSpec {
  type MR = MarketManager.MatchResult

  implicit var timeProvider = new TimeProvider {
    def getCurrentTimeMillis = -1
  }

  var marketId = MarketId(GTO, WETH)
  var config = MarketManagerConfig(
    maxNumbersOfOrders = 100, /* unsupported */
    priceDecimals = 5
  )

  var fackRingMatcher: RingMatcher = _
  var fakeDustOrderEvaluator: DustOrderEvaluator = _
  var fakePendingRingPool: PendingRingPool = _
  var marketManager: MarketManager = _

  override def beforeEach() {
    fackRingMatcher = stub[RingMatcher]
    fakeDustOrderEvaluator = stub[DustOrderEvaluator]
    fakePendingRingPool = stub[PendingRingPool]

    marketManager = new MarketManagerImpl(
      marketId,
      config,
      tmm,
      fackRingMatcher,
      fakePendingRingPool,
      fakeDustOrderEvaluator
    )
  }
}
