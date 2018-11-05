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

trait DustOrderEvaluator {
  def isDust(order: Order): Boolean // TODO(dong): this method is confusing and should be removed.

  def isOriginalDust(order: Order): Boolean
  def isOutstandingDust(order: Order): Boolean
  def isActualDust(order: Order): Boolean
  def isMathcableDust(order: Order): Boolean
}

class DustOrderEvaluatorImpl(threshold: Double)(
    implicit
    tve: TokenValueEstimator
)
  extends DustOrderEvaluator {

  def isDust(order: Order): Boolean = isOriginalDust(order)

  def isOriginalDust(order: Order) = _isDust(order.tokenS, order.matchable.amountS)
  def isOutstandingDust(order: Order) = _isDust(order.tokenS, order.matchable.amountS)
  def isActualDust(order: Order) = _isDust(order.tokenS, order.matchable.amountS)
  def isMathcableDust(order: Order) = _isDust(order.tokenS, order.matchable.amountS)

  private def _isDust(tokenS: Address, amountS: Amount): Boolean = {
    tve.getFiatValue(tokenS, amountS) < threshold
  }
}
