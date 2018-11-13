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

package org.loopring.lightcone.core.testkit

import org.scalatest.FlatSpec

trait EventsBehaviors { this: FlatSpec ⇒

  def batchTest(events: Seq[Event]): Unit = {
    events foreach {
      event ⇒
        info(event.info)
        event.tell()
        Thread.sleep(event.timeout)
        event.asserts.foreach {
          a ⇒
            {
              a()
              //              var i = 0
              //              var pass = false
              //              //因为event可能在future中执行，因此等待直到预期效果，或者超时
              //              while (i < 50 && !pass) {
              //                Thread.sleep(10)
              //                i += 1
              //              }
              //              assert(pass)
            }
        }
    }
  }

}
