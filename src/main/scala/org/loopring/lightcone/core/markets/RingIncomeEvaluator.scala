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

trait RingIncomeEstimator {
  def getIncomeFiatValue(ring: Ring): Double
  def isProfitable(ring: Ring): Boolean
}

final class RingIncomeEstimatorImpl(
    threshold: Double
)(implicit tve: TokenValueEstimator) extends RingIncomeEstimator {

  def getIncomeFiatValue(ring: Ring) = ring.maker.getIncomeFiatValue() + ring.taker.getIncomeFiatValue()

  def isProfitable(ring: Ring) = getIncomeFiatValue(ring) >= threshold
}
