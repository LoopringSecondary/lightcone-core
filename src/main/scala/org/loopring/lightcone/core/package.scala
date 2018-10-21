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

import java.math.BigInteger

import org.web3j.utils.Numeric

package object core {
  type Amount = BigInt
  type Address = String
  type ID = String
  type RingID = Array[Byte]

  implicit class RichAmount(this_ : Amount) {
    def ÷(that: Amount): Double = (BigDecimal(this_) / BigDecimal(that)).toDouble
    def ×(d: Double): Amount = (BigDecimal(this_) * BigDecimal(d)).toBigInt
    def min(that: Amount): Amount = if (this_ < that) this_ else that
    def max(that: Amount): Amount = if (this_ > that) this_ else that
  }

  implicit class RichByteArray(this_ : Array[Byte]) {
    def addAddress(address: String): Array[Byte] = this_ ++ Numeric.toBytesPadded(Numeric.toBigInt(address), 20)

    def addUint256(num: BigInteger): Array[Byte] = this_ ++ Numeric.toBytesPadded(num, 32)

    def addUint16(num: BigInteger): Array[Byte] = this_ ++ Numeric.toBytesPadded(num, 2)

    def addBoolean(b: Boolean): Array[Byte] = this_ :+ (if (b) 1 else 0).toByte

    def addRawBytes(otherBytes: Array[Byte]): Array[Byte] = this_ ++ otherBytes

    def addHex(hex: String): Array[Byte] = this_ ++ Numeric.hexStringToByteArray(hex)
  }
}
