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

package object helper {

  val lrc = "LRC"
  val xyz = "XYZ"
  val gto = "GTO"
  val eth = "WETH"

  def prepare = {
    implicit val tokenValueEstimator = new TokenValueEstimatorImpl()
    tokenValueEstimator.setTokens(Map(
      lrc -> 1,
      xyz -> 1,
      gto -> 1
    ))
    tokenValueEstimator.setMarketCaps(Map(
      lrc -> 1,
      xyz -> 1,
      gto -> 1
    ))

    implicit val dustEvaluator = new DustOrderEvaluatorImpl(0)
    implicit val orderPool = new OrderPoolImpl()

    val manager = OrderManager.default()

    manager.addTokenManager(new TokenManager(lrc))
    manager.addTokenManager(new TokenManager(xyz))
    manager.addTokenManager(new TokenManager(gto))

    val lrcTokenManager = manager.getTokenManager(lrc)
    val xyzTokenManager = manager.getTokenManager(xyz)
    val gtoTokenManager = manager.getTokenManager(gto)

    (manager, orderPool, lrcTokenManager, xyzTokenManager, gtoTokenManager)
  }
}
