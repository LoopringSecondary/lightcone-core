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

import scala.collection.immutable.HashMap

trait TokenValueEstimator {
  def getFiatValue(token: Address, amount: Amount): Double

  def getBurnRate(token: Address): Double

  def canGetMarketcap(token: Address): Boolean
}

class TokenValueEstimatorImpl extends TokenValueEstimator {

  var tokens = HashMap[Address, BigInt]()

  var marketcaps = HashMap[Address, Double]()

  var burnRates = HashMap[Address, Double]()

  def resetTokens(tokens: HashMap[Address, BigInt]): Unit = {
    this.tokens = tokens
  }

  def resetMarketcaps(marketcaps: HashMap[Address, Double]): Unit = {
    this.marketcaps = marketcaps
  }

  def resetBurnRates(burnRates: HashMap[Address, Double]): Unit = {
    this.burnRates = burnRates
  }

  override def getFiatValue(token: Address, amount: Amount): Double = {
    val decimal = tokens.getOrElse(token, BigInt(1))
    val price = marketcaps.getOrElse(token, 0.0)
    price * amount.doubleValue() / decimal.doubleValue()
  }

  override def getBurnRate(token: Address): Double = burnRates.getOrElse(token, 0.05)

  override def canGetMarketcap(token: Address): Boolean = tokens.contains(token) && marketcaps.contains(token)
}
