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

package org.loopring.lightcone.core.demo

private[core] final case class Item(id: String, amount: Int, reserved: Int = 0)

private[core] final case class Reserve(
    id: String,
    accumulatedBalance: Int = -1,
    accumulatedAllowance: Int = -1
)

private[core] final class DemoManager() {
  private[core] var balance: Int = 0
  private[core] var allowance: Int = 0

  private[core] var items: Map[String, Item] = Map.empty
  private[core] var idxMap: Map[String, Int] = Map.empty
  private[core] var reservations: Seq[Reserve] = Seq.empty
  private[core] var cursor: Int = -1

  def getLocalItems() = reservations.map(r ⇒ items(r.id))

  def getAccumulatedAtCursor(): (Int, Int) = {
    if (cursor < 0) (0, 0)
    else {
      val r = reservations(cursor)
      (r.accumulatedBalance, r.accumulatedAllowance)
    }
  }

  def initiate(balance_ : Int, allowance_ : Int) = {
    val cursor1 =
      if (balance_ >= balance) cursor
      else {
        val idx = reservations.indexWhere { r ⇒
          r.accumulatedBalance > balance_
        }
        if (idx == -1) cursor else idx - 1
      }

    val cursor2 =
      if (allowance_ >= allowance) {
        val idx = reservations.indexWhere { r ⇒
          val item = items(r.id)
          item.reserved != item.amount
        }
        if (idx == -1) cursor else idx - 1
      } else {
        val idx = reservations.indexWhere { r ⇒
          r.accumulatedAllowance > allowance_
        }
        if (idx == -1) cursor else idx - 1
      }

    cursor = Math.min(cursor1, cursor2)
    balance = balance_
    allowance = allowance_

    rebalance()
  }

  def add(item: Item) {
    items += item.id -> item.copy(reserved = 0)
    reservations :+= Reserve(item.id)
    rebalance()
  }

  def increaseItemSize(id: String, delta: Int) =
    resizeInternal(id, delta, _.amount + delta)

  def resize(id: String, newAmount: Int) =
    resizeInternal(id, newAmount, _ ⇒ newAmount)

  private def resizeInternal(
    id: String,
    value: Int,
    getNewAmount: Item ⇒ Int
  ): Boolean = {
    idxMap.get(id) match {
      case None ⇒ false
      case Some(idx) ⇒
        assert(items.contains(id))
        cursor = Math.min(cursor, idx)

        val item = items(id)
        items += item.id -> item.copy(amount = getNewAmount(item))

        cursor = idx - 1
        rebalance()
        true
    }
  }

  def remove(id: String): Boolean = {
    idxMap.get(id) match {
      case None ⇒ false
      case Some(idx) ⇒
        var r = reservations(idx)
        reservations = reservations.slice(0, idx) ++
          reservations.slice(idx + 1, reservations.size)
        items -= r.id
        cursor = idx - 1
        rebalance()
        true
    }
  }

  private def rebalance() {
    val (goodOnes, badOnes) = reservations.splitAt(cursor + 1)
    reservations = goodOnes

    // println("---")
    // println("good ones: " + goodOnes)
    // println("badOnes: " + badOnes)
    // println("balance: " + balance)
    // println("allowance: " + allowance)

    var (accumulatedBalance, accumulatedAllowance) = getAccumulatedAtCursor()

    // println("accumulatedBalance: " + accumulatedBalance)
    // println("accumulatedAllowance: " + accumulatedAllowance)

    var availableBalance = balance - accumulatedBalance
    var availableAllowance = allowance - accumulatedAllowance

    // println("availableBalance: " + availableBalance)
    // println("availableAllowance: " + availableAllowance)

    badOnes.foreach { r ⇒
      val item = items(r.id)

      // println("---")
      // println("availableBalance: " + availableBalance)
      // println("availableAllowance: " + availableAllowance)

      if (availableBalance < item.amount) {
        items -= item.id
        idxMap -= item.id
      } else {
        val reserved =
          if (availableAllowance >= item.amount) item.amount
          else availableAllowance

        // println("reserved:" + reserved)

        accumulatedBalance += item.amount
        accumulatedAllowance += reserved

        availableBalance = balance - accumulatedBalance
        availableAllowance = allowance - accumulatedAllowance

        idxMap += item.id -> reservations.size
        items += item.id -> item.copy(reserved = reserved)
        reservations :+= Reserve(item.id, accumulatedBalance, accumulatedAllowance)
        cursor += 1
      }
    }
  }
}
