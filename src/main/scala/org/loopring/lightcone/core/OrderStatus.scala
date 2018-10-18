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

final object OrderStatus extends Enumeration {
  type OrderStatus = Value

  val NEW = Value(0)
  val PENDING = Value(1)
  val CANCELLED_LOW_BALANCE = Value(2)
  val CANCELLED_LOW_FEE_BALANCE = Value(3)
  val CANCELLED_BY_USER = Value(4)
  val EXPIRED = Value(5)
}
