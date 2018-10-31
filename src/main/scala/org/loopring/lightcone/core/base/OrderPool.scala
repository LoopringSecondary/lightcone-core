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

trait OrderPool[T] extends Object {

  type Callback = T ⇒ Unit

  private var callbacks = Seq.empty[Callback]
  private var orderMap = Map.empty[ID, T]

  def apply(id: ID): T = orderMap(id)
  def getOrder(id: ID): Option[T] = orderMap.get(id)
  def contains(id: ID): Boolean = orderMap.contains(id)
  def orders() = orderMap.values
  def add(id: ID, order: T) = orderMap += id -> order
  def del(id: ID) = orderMap -= id

  def addCallback(callback: Callback) = {
    callbacks :+= callback
    callbacks
  }

  def removeCallback(callback: Callback) = {
    callbacks = callbacks.dropWhile(_ == callback)
    callbacks
  }

  def callback(t: T): Unit = callbacks.foreach(_(t))

  def +=(t: T): Unit
}
