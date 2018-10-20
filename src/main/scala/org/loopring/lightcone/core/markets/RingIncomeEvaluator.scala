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

trait RingIncomeEstimator[T] {
  def getFiatValue(ring: Ring[T]): Double
  def isProfitable(ring: Ring[T]): Boolean
}

final class RingIncomeEstimatorImpl[T](
    threshold: Double,
    tve: TokenValueEstimator
) extends RingIncomeEstimator[T] {

  def getFiatValue(ring: Ring[T]) = {
    ring.expectedFills.map { fill ⇒
      val order = fill.order
      tve.getFiatValue(
        order.tokenFee.getOrElse(order.tokenS),
        fill.pending.amountFee
      ) +
        tve.getFiatValue(
          order.tokenS,
          fill.amountMargin
        )
    }.sum
  }

  def isProfitable(ring: Ring[T]) = getFiatValue(ring) >= threshold
}
