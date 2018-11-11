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

package org.loopring.lightcone.core.order

import org.loopring.lightcone.core.data._

class TokenMetadataManager {

  private var tokens = Map.empty[String, TokenMetadata]

  private var decimalsMap = Map.empty[String, Int]
  private var priceMap = Map.empty[String, Double]
  private var burnRateMap = Map.empty[String, Double]

  def addToken(token: TokenMetadata) {
    tokens += token.address -> token
  }

  def hasToken(token: String) = tokens.contains(token)

  def updatePrices(priceMap: Map[String, Double]) {
    tokens = tokens.map {
      case (address, token) ⇒ priceMap.get(address) match {
        case Some(price) ⇒ (address, token.copy(currentPrice = price))
        case None        ⇒ (address, token)
      }
    }
  }

  def setBurnRates(burnRateMap: Map[String, Double]) {
    this.burnRateMap = burnRateMap
  }

  def getFiatValue(token: String, amount: BigInt): Double = {
    if (amount.signum <= 0) 0
    else {
      val decimals: Int = decimalsMap.getOrElse(token, 0)
      val scaling: Double = Math.pow(10, decimals)
      val price = priceMap.getOrElse(token, 0.0)
      (Rational(price) * Rational(amount) / Rational(scaling)).doubleValue
    }
  }

  def getBurnRate(token: String): Double = burnRateMap.getOrElse(token, 0.05)
}
