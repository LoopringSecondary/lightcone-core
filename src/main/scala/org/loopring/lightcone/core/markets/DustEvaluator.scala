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

package org.loopring.lightcone.core.markets

import org.loopring.lightcone.core.{ Order, TokenValueEstimator }

trait DustEvaluator {
  def isDust(order: Order): Boolean
}

class DustEvaluatorImpl(threshold: Double)(implicit tve: TokenValueEstimator) extends DustEvaluator {

  override def isDust(order: Order): Boolean = {
    val fiatValue = tve.getFiatValue(order.tokenS, order.matchable.amountS)
    fiatValue < threshold
  }

}
