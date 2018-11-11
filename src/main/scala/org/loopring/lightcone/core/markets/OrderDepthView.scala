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

// class MarketDepthView(
//     marketId: MarketId,
//     primaryTokenDecimals: Int,
//     secondaryTokenDecimals: Int,
//     priceDecimals: Int, levels: Int
// ) extends DepthView2(priceDecimals, levels) {

//   private val primaryScaling = Rational(Math.pow(10, primaryTokenDecimals))
//   private val secondaryScaling = Rational(Math.pow(10, secondaryTokenDecimals))

//   def addOrder(order: Order) = handleOrder(order, false)
//   def removeOrder(order: Order) = handleOrder(order, true)

//   private def handleOrder(order: Order, delete: Boolean) = {
//     val (isSell, amount, total) =
//       if (order.tokenS == marketId.secondary)
//         (true,
//           Rational(order.amountS) / secondaryScaling,
//           Rational(order.amountB) / primaryScaling)
//       else
//         (false,
//           Rational(order.amountS) / primaryScaling,
//           Rational(order.amountB) / secondaryScaling)

//     val price = Rational(amount, total).doubleValue
//     val mul = if (delete) -1 else 1
//     addItem(
//       isSell,
//       price,
//       amount.doubleValue * mul,
//       total.doubleValue * mul
//     )
//   }
// }
