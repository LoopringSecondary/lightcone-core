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

package org.loopring.lightcone

package object core {
  type Amount = BigInt
  type Address = String
  type ID = String
  type RingID = String

  implicit class RichAmount(this_ : Amount) {
    def ÷(that: Amount): Double = (BigDecimal(this_) / BigDecimal(that)).toDouble
    def ×(d: Double): Amount = (BigDecimal(this_) * BigDecimal(d)).toBigInt
    def min(that: Amount): Amount = if (this_ < that) this_ else that
    def max(that: Amount): Amount = if (this_ > that) this_ else that
  }

  implicit class RichDouble(d: Double) {
    def scaled(s: Int) = BigDecimal(d).setScale(s, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

}
