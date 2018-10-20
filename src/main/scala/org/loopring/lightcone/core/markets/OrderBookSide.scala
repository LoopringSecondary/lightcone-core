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

import scala.collection.mutable.SortedSet
import org.slf4s.Logging

object OrderBookSide {

  private def defaultOrdering[T]() = new Ordering[Order[T]] {
    def compare(a: Order[T], b: Order[T]) = {
      if (a.rate < b.rate) 1
      else if (a.rate > b.rate) -1
      else if (a.createdAt < b.createdAt) 1
      else if (a.createdAt > b.createdAt) -1
      else 0
    }
  }
}

class OrderBookSide[T](tokenS: Address)(implicit orderPool: OrderPool[T])
  extends Object with Logging {
  import OrderBookSide._

  private[core] implicit val ordering = defaultOrdering[T]()
  private[core] val orders = SortedSet.empty[Order[T]]

  var bestPrice: Option[Rational] = None

  def getTops(num: Int, skip: Int = 0, includingHidden: Boolean = false): Seq[Order[T]] = ???

}
