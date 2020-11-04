/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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

package dev.tauri.choam
package kcas

import java.util.concurrent.{ CountDownLatch, CyclicBarrier }

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals

final class IBRStackSpec
  extends AnyFlatSpec
  with Matchers
  with TypeCheckedTripleEquals {

  "IBRStack" should "work" in {
    val s = IBRStackDebug[Int]()
    s.push(1)
    s.push(2)
    s.push(3)
    assert(s.tryPop().get === 3)
    assert(s.tryPop().get === 2)
    assert(s.tryPop().get === 1)
    assert(s.tryPop().isEmpty)
  }

  it should "reuse nodes from the freelist" in {
    val N = 42
    val SYNC = 128
    val s = IBRStackDebug[Int]()
    for (i <- 1 to (16 * IBR.emptyFreq)) {
      s.push(i)
    }
    val latch = new CountDownLatch(3)
    val barrier = new CyclicBarrier(2)
    val pusher = new Thread(() => {
      latch.countDown()
      latch.await()
      for (i <- 1 to (16 * IBR.emptyFreq)) {
        s.push(i)
        if ((i % SYNC) == 0) {
          barrier.await()
        }
      }
    })
    pusher.start()
    val popper = new Thread(() => {
      latch.countDown()
      latch.await()
      for (i <- 1 to (16 * IBR.emptyFreq)) {
        s.tryPop().get
        if ((i % SYNC) == 0) {
          barrier.await()
        }
      }
      for (_ <- 1 to N) {
        s.push(42)
      }
    })
    popper.start()
    latch.countDown()
    latch.await()
    pusher.join()
    popper.join()
    assert(s.reusedCount >= (N/2)) // the exact count is non-deterministic
  }

  it should "copy itself to a List" in {
    val s = IBRStackDebug[Int]()
    s.push(1)
    s.push(2)
    s.push(3)
    assert(s.unsafeToList() === List(3, 2, 1))
  }
}
