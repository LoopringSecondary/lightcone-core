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

package org.loopring.lightcone.core.testkit

import org.loopring.lightcone.core.data.MatchingFailure.MatchingFailure
import org.loopring.lightcone.core.data._
import org.scalatest._

trait RingMatcherAssert extends Matchers { this: FlatSpec ⇒

  def shouldNotTradable(res: Either[MatchingFailure, OrderRing]): Unit = {
    res should be(Left(MatchingFailure.ORDERS_NOT_TRADABLE))
  }

  def shouldIncomeTooSmall(res: Either[MatchingFailure, OrderRing]): Unit = {
    res should be(Left(MatchingFailure.INCOME_TOO_SMALL))
  }

  def shouldRing(res: Either[MatchingFailure, OrderRing], expectRingOpt: Option[OrderRing] = None): Unit = {
    expectRingOpt match {
      case None             ⇒ res.isRight should be(true)
      case Some(expectRing) ⇒ res should be(Right(expectRing))
    }
  }

}
