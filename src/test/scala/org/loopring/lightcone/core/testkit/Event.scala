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

import org.loopring.lightcone.core.data.Order

trait Event {
  val info: String
  val event: Any
  val asserts: Seq[() ⇒ Unit]
  val tell: () ⇒ Unit
  val timeout: Long = 0
}

case class OrderEvent(
    info: String,
    event: Order,
    asserts: Seq[() ⇒ Unit],
    tell: () ⇒ Unit
) extends Event

case class MatchingOrderEvent(
    info: String,
    event: (Order, Order, Double),
    asserts: Seq[() ⇒ Unit],
    tell: () ⇒ Unit
) extends Event
