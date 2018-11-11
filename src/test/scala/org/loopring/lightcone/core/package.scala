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

package org.loopring.lightcone

import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.order._
import org.loopring.lightcone.core.market._

package object core {

  val lrc = "LRC"
  val xyz = "XYZ"
  val gto = "GTO"
  val eth = "WETH"

  def prepare = {

    implicit val tmm = new TokenMetadataManager()
    tmm.addToken(TokenMetadata(lrc, 1, 0, 1))
    tmm.addToken(TokenMetadata(eth, 1, 0, 1))
    tmm.addToken(TokenMetadata(gto, 1, 0, 1))

    implicit val dustEvaluator = new DustOrderEvaluatorImpl(0)
    implicit val orderPool = new OrderPool()

    val manager = OrderManager.default()

    manager.addTokenReserveManager(new TokenReserveManager(lrc))
    manager.addTokenReserveManager(new TokenReserveManager(xyz))
    manager.addTokenReserveManager(new TokenReserveManager(gto))

    val lrcTokenReserveManager = manager.getTokenReserveManager(lrc)
    val xyzTokenReserveManager = manager.getTokenReserveManager(xyz)
    val gtoTokenReserveManager = manager.getTokenReserveManager(gto)

    (manager, orderPool, lrcTokenReserveManager, xyzTokenReserveManager, gtoTokenReserveManager)
  }
}
