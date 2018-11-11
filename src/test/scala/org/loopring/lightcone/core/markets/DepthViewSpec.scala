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

import org.scalatest._

class DepthViewSpec extends FlatSpec with Matchers {

  "DepthView" should "manage depths data in multiple levels" in {
    val dv = new DepthView(6, 4) // 6, 5, 4, 3

    var data = dv.getDepthData(0, 0, 100)
    data should be(DepthData(List(), List()))

    dv.addItem(true, 0.45678901234567, 1, 2)
    dv.addItem(true, 0.45678901234567, 3, 4)

    dv.addItem(false, 0.35678901234567, 10, 20)
    dv.addItem(false, 0.35678901234567, 30, 40)

    data = dv.getDepthData(0, 0, 100)
    data should be(DepthData(List(), List()))

    data = dv.getDepthData(1, 0, 100)
    data should be(DepthData(List(), List()))

    data = dv.getDepthData(2, 0, 100)
    data should be(DepthData(List(), List()))

    data = dv.getDepthData(7, 0, 100)
    data should be(DepthData(List(), List()))

    data = dv.getDepthData(8, 0, 100)
    data should be(DepthData(List(), List()))

    data = dv.getDepthData(6, 0.4, 100)
    data should be(DepthData(
      Seq(DepthItem(0.456790, 4.0, 6.0)),
      Seq(DepthItem(0.356789, 40.0, 60.0))
    ))

    data = dv.getDepthData(5, 0.4, 100)
    data should be(DepthData(
      Seq(DepthItem(0.45679, 4.0, 6.0)),
      Seq(DepthItem(0.35678, 40.0, 60.0))
    ))

    data = dv.getDepthData(4, 0.4, 100)
    data should be(DepthData(
      Seq(DepthItem(0.4568, 4.0, 6.0)),
      Seq(DepthItem(0.3567, 40.0, 60.0))
    ))
  }

  "DepthView" should "return list data" in {
    val dv = new DepthView(7, 4) // 6, 5, 4, 3

    dv.addItem(true, 0.1000111, 1, 2)
    dv.addItem(true, 0.1000112, 1, 2)
    dv.addItem(true, 0.1000113, 1, 2)
    dv.addItem(true, 0.1000114, 1, 2)
    dv.addItem(true, 0.1000115, 1, 2)

    dv.addItem(false, 0.1000009, 10, 20)
    dv.addItem(false, 0.1000008, 10, 20)
    dv.addItem(false, 0.1000007, 10, 20)
    dv.addItem(false, 0.1000006, 10, 20)
    dv.addItem(false, 0.1000005, 10, 20)

    var data = dv.getDepthData(7, 0.1000111, 3)

    data should be(DepthData(
      Seq(
        DepthItem(0.1000111, 1, 2),
        DepthItem(0.1000112, 2, 4),
        DepthItem(0.1000113, 3, 6)
      ),
      Seq(
        DepthItem(0.1000009, 10, 20),
        DepthItem(0.1000008, 20, 40),
        DepthItem(0.1000007, 30, 60)
      )
    ))

    data = dv.getDepthData(6, 0.1000111, 3)

    data should be(DepthData(
      Seq(
        DepthItem(0.100012, 5, 10)
      ),
      Seq(
        DepthItem(0.100000, 50, 100)
      )
    ))
  }
}
