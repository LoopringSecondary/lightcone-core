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

import org.scalatest._

class DemoManagerSpec extends FlatSpec with Matchers {

  "manager" should "can resize items" in {
    val manager = new DemoManager()
    manager.initiate(100, 200)

    (0 until 20) foreach { i ⇒
      manager.add(Item(s"i$i", 10))
    }

    manager.resize("i0", 5)

    manager.resize("i0", 50)

    manager.resize("i0", 95)

    manager.resize("i0", 100)

    manager.resize("i0", 1)
  }

  "manager" should "can remove items and auto rebalance" in {
    val manager = new DemoManager()
    manager.initiate(100, 0)

    (0 until 20) foreach { i ⇒
      manager.add(Item(s"i$i", 10))
    }

    manager.initiate(100, 55)
    manager.initiate(100, 5)
    manager.initiate(100, 200)
    manager.initiate(45, 200)
    manager.initiate(45, 0)

    manager.getLocalItems should be(Seq(
      Item("i0", 10, 0),
      Item("i1", 10, 0),
      Item("i2", 10, 0),
      Item("i3", 10, 0)
    ))

    manager.remove("not exist")
    manager.remove("i1")
    manager.remove("i3")
    manager.remove("i2")
    manager.remove("i0")

    manager.getLocalItems should be(Nil)
    manager.reservations should be(Nil)
    manager.items should be(Map.empty)

    (0 until 20) foreach { i ⇒
      manager.add(Item(s"i$i", 10))
    }

    manager.getLocalItems should be(Seq(
      Item("i0", 10, 0),
      Item("i1", 10, 0),
      Item("i2", 10, 0),
      Item("i3", 10, 0)
    ))

    // manager.getLocalItems.foreach(println)
    // manager.reservations.foreach(println)

  }
  "manager" should "can add and remove items correctly" in {
    val manager = new DemoManager()
    manager.initiate(100, 60)

    manager.add(Item("a", 80))
    manager.reservations should be(Seq(
      Reserve("a", 80, 60)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 60)
    ))

    manager.add(Item("b", 10))
    manager.reservations should be(Seq(
      Reserve("a", 80, 60),
      Reserve("b", 90, 60)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 60),
      "b" -> Item("b", 10, 0)
    ))

    manager.initiate(200, 200)
    manager.reservations should be(Seq(
      Reserve("a", 80, 80),
      Reserve("b", 90, 90)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 80),
      "b" -> Item("b", 10, 10)
    ))

    manager.initiate(100, 100)
    manager.reservations should be(Seq(
      Reserve("a", 80, 80),
      Reserve("b", 90, 90)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 80),
      "b" -> Item("b", 10, 10)
    ))

    manager.initiate(100, 90)
    manager.reservations should be(Seq(
      Reserve("a", 80, 80),
      Reserve("b", 90, 90)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 80),
      "b" -> Item("b", 10, 10)
    ))

    manager.initiate(100, 85)
    manager.reservations should be(Seq(
      Reserve("a", 80, 80),
      Reserve("b", 90, 85)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 80),
      "b" -> Item("b", 10, 5)
    ))

    manager.initiate(100, 95)
    manager.reservations should be(Seq(
      Reserve("a", 80, 80),
      Reserve("b", 90, 90)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 80),
      "b" -> Item("b", 10, 10)
    ))

    manager.initiate(90, 60)
    manager.reservations should be(Seq(
      Reserve("a", 80, 60),
      Reserve("b", 90, 60)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 60),
      "b" -> Item("b", 10, 0)
    ))

    manager.initiate(85, 100)
    manager.reservations should be(Seq(
      Reserve("a", 80, 80)
    ))
    manager.items should be(Map(
      "a" -> Item("a", 80, 80)
    ))
  }
}
