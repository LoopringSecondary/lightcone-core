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

package org.loopring.lightcone.core.order

import org.loopring.lightcone.core.base._
import org.loopring.lightcone.core.data._
import org.loopring.lightcone.core.order._
import org.scalatest._
import org.slf4s.Logging

class CommonSpec
  extends FlatSpec
  with BeforeAndAfterEach
  with BeforeAndAfterAll
  with Matchers
  with Logging {

  val rand = new scala.util.Random

  val LRC = "LRC"
  val GTO = "GTO"
  val DAI = "DAI"
  val WETH = "WETH"

  val LRC_TOKEN = TokenMetadata(LRC, 0, 0.1, 1.0)
  val GTO_TOKEN = TokenMetadata(GTO, 1, 0.2, 1400.0)
  val DAI_TOKEN = TokenMetadata(DAI, 2, 0.3, 7.0)
  val WETH_TOKEN = TokenMetadata(WETH, 3, 0.4, 0.5)

  implicit val tmm = new TokenMetadataManager()
  tmm.addToken(LRC_TOKEN)
  tmm.addToken(GTO_TOKEN)
  tmm.addToken(DAI_TOKEN)
  tmm.addToken(WETH_TOKEN)

  implicit val dustEvaluator = new DustOrderEvaluatorImpl

  implicit var orderPool: OrderPool = _
  var orderManager: OrderManager = _
  var lrc: TokenReserveManager = _
  var gto: TokenReserveManager = _
  var dai: TokenReserveManager = _
  var weth: TokenReserveManager = _

  var updatedOrders = Map.empty[String, Order]

  override def beforeAll() {
    println(s"[To run this spec, use `testOnly *${getClass.getSimpleName}`]")
  }

  override def beforeEach() {
    orderPool = new OrderPool()
    updatedOrders = Map.empty[String, Order]
    orderPool.addCallback { order ⇒
      updatedOrders += order.id -> order
      // println("----UO: " + order)
      // log.debug("order: " + order)
    }
    orderManager = OrderManager.default()

    lrc = new TokenReserveManager(LRC)
    gto = new TokenReserveManager(GTO)
    dai = new TokenReserveManager(DAI)
    weth = new TokenReserveManager(WETH)

    orderManager.addTokenReserveManager(lrc)
    orderManager.addTokenReserveManager(gto)
    orderManager.addTokenReserveManager(dai)
    orderManager.addTokenReserveManager(weth)
  }

  def sellLRC(
    amountS: Long,
    amountB: Long,
    amountFee: Long = 0
  ) = newOrder(LRC, WETH, LRC, amountS, amountB, amountFee)

  def buyLRC(
    amountS: Long,
    amountB: Long,
    amountFee: Long = 0
  ) = newOrder(WETH, LRC, LRC, amountS, amountB, amountFee)

  def sellDAI(
    amountS: Long,
    amountB: Long,
    amountFee: Long = 0
  ) = newOrder(DAI, WETH, LRC, amountS, amountB, amountFee)

  def buyDAI(
    amountS: Long,
    amountB: Long,
    amountFee: Long = 0
  ) = newOrder(WETH, DAI, LRC, amountS, amountB, amountFee)

  def newOrder(
    tokenS: String,
    tokenB: String,
    tokenFee: String,
    amountS: Long,
    amountB: Long,
    amountFee: Long = 0
  ) = Order(
    rand.nextLong().toString,
    tokenS,
    tokenB,
    tokenFee,
    BigInt(amountS),
    BigInt(amountB),
    BigInt(amountFee)
  )

  def orderState(
    amountS: Long,
    amountB: Long,
    amountFee: Long
  ) = OrderState(BigInt(amountS), BigInt(amountB), BigInt(amountFee))

  def submitOrder(order: Order) = {
    updatedOrders = Map.empty[String, Order]
    orderManager.submitOrder(order)
  }

  def cancelOrder(orderId: String) = {
    updatedOrders = Map.empty[String, Order]
    orderManager.cancelOrder(orderId)
  }

  def adjustOrder(orderId: String, outstandingAmountS: Long) = {
    updatedOrders = Map.empty[String, Order]
    orderManager.adjustOrder(orderId, BigInt(outstandingAmountS))
  }

  def resetUpdatedOrders() {
    updatedOrders = Map.empty[String, Order]
  }
}
