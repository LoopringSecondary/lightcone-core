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
trait TokenValueEstimator {
  def getFiatValue(token: Address, amount: Amount): Double

  def getEthFiatValue(amount: Amount): Double

  def getBurnRate(token: Address): Double

  def canGetMarketCap(token: Address): Boolean
}

class TokenValueEstimatorImpl extends TokenValueEstimator {

  var tokens = Map[Address, BigInt]() // map[Address, Decimal]
  var marketcaps = Map[Address, Double]() // map[Address, price]
  var burnRates = Map[Address, Double]() // ma[Address, burnRate]

  def setTokens(tokens: Map[Address, BigInt]): Unit = {
    this.tokens = tokens
  }

  def setMarketCaps(marketcaps: Map[Address, Double]): Unit = {
    this.marketcaps = marketcaps
  }

  def setBurnRates(burnRates: Map[Address, Double]): Unit = {
    this.burnRates = burnRates
  }

  def getFiatValue(token: Address, amount: Amount): Double = {
    if (amount.signum <= 0) {
      0
    } else {
      val decimal = tokens.getOrElse(token, BigInt(1))
      val price = marketcaps.getOrElse(token, 0.0)
      price * amount.doubleValue() / decimal.doubleValue()
    }
  }

  def getEthFiatValue(amount: Amount): Double = {
    if (amount.signum <= 0) {
      0
    } else {
      val decimal = tokens.getOrElse("ETH", BigInt(1)) //todo:需要确定eth的地址
      val price = marketcaps.getOrElse("ETH", 0.0)
      price * amount.doubleValue() / decimal.doubleValue()
    }
  }

  def getBurnRate(token: Address): Double = burnRates.getOrElse(token, 0.05)

  def canGetMarketCap(token: Address): Boolean = tokens.contains(token) && marketcaps.contains(token)
}

