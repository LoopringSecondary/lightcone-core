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

trait DustEvaluator {
  def isDust(order: Order): Boolean
  def isDust(token: Address, availableBalance: Amount): Boolean
  def isDust(token: Address, availableBalance: Amount, requestedAmount: Amount): Boolean
}

final class DustEvaluatorImpl extends DustEvaluator {

  // todo: 判断是否为灰尘单
  def isDust(order: Order): Boolean = {
    order.reservedAmount()(order.tokenS) <= 0
  }

  // todo: 判断订单余额/账户金额是否可用
  def isDust(token: Address, amount: Amount): Boolean = {
    amount <= 0
  }

  def isDust(token: Address, availableBalance: Amount, requestedAmount: Amount): Boolean = {
    if (availableBalance < requestedAmount) true
    else {
      isDust(token, availableBalance) || isDust(token, requestedAmount)
    }
  }
}
