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

import org.loopring.lightcone.core.ExecutedStatus.ExecutedStatus
import scala.collection.mutable.HashMap
/** 记录订单以及环路的执行情况，尤其失败的次数
 */

final object ExecutedStatus extends Enumeration {
  type ExecutedStatus = Value

  val FAILED = Value
  val SUCCESS = Value
}

trait ExecutedStatusHolder {
  def addInChainStatus(hash: ID, status: ExecutedStatus): Unit
  def addPreStatus(hash: ID, status: ExecutedStatus): Unit
  def getOnChainFailedCount(hash: ID): Int
  def getPreFailedCount(hash: ID): Int
}

class ExecutedStatusHolderImp extends ExecutedStatusHolder {
  val ringOrOrderOnChainFailed = HashMap.empty[ID, Int]
  val ringOrOrderPreFailed = HashMap.empty[ID, Int]

  def addInChainStatus(hash: ID, status: ExecutedStatus): Unit = {
    status match {
      case ExecutedStatus.FAILED ⇒
        val cnt = ringOrOrderOnChainFailed.getOrElse(hash, 0)
        ringOrOrderOnChainFailed.put(hash, cnt + 1)
      case _ ⇒
    }
  }

  def addPreStatus(hash: ID, status: ExecutedStatus): Unit =
    status match {
      case ExecutedStatus.FAILED ⇒
        val cnt = ringOrOrderPreFailed.getOrElse(hash, 0)
        ringOrOrderPreFailed.put(hash, cnt + 1)
      case _ ⇒
    }

  def getOnChainFailedCount(hash: ID): Int = ringOrOrderOnChainFailed.getOrElse(hash, 0)

  def getPreFailedCount(hash: ID): Int = ringOrOrderPreFailed.getOrElse(hash, 0)
}
