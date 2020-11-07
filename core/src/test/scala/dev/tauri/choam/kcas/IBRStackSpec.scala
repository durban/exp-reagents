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
    val s = IBRStackDebug[String]()
    val tc = IBRStackDebug.threadLocalContext[String]()
    s.push("a", tc)
    s.push("b", tc)
    s.push("c", tc)
    assert(s.tryPop(tc) === "c")
    assert(s.tryPop(tc) === "b")
    assert(s.tryPop(tc) === "a")
    assert(Option(s.tryPop(tc)).isEmpty)
  }

  it should "reuse nodes from the freelist" in {
    val N = 42L
    val SYNC = 128L
    val s = IBRStackDebug[String]()
    val tc = IBRStackDebug.threadLocalContext[String]()
    for (i <- 1 to (16 * IBR.emptyFreq)) {
      s.push(i.toString, tc)
    }
    val latch = new CountDownLatch(3)
    val barrier = new CyclicBarrier(2)
    val pusher = new Thread(() => {
      val tc = IBRStackDebug.threadLocalContext[String]()
      latch.countDown()
      latch.await()
      for (i <- 1 to (16 * IBR.emptyFreq)) {
        s.push(i.toString, tc)
        if ((i % SYNC) == 0) {
          barrier.await()
        }
      }
    })
    pusher.start()
    val popper = new Thread(() => {
      val tc = IBRStackDebug.threadLocalContext[String]()
      latch.countDown()
      latch.await()
      for (i <- 1 to (16 * IBR.emptyFreq)) {
        assert(Option(s.tryPop(tc)).nonEmpty)
        if ((i % SYNC) == 0) {
          barrier.await()
        }
      }
      for (_ <- 1L to N) {
        s.push("42", tc)
      }
    })
    popper.start()
    latch.countDown()
    latch.await()
    pusher.join()
    popper.join()
    assert(s.debugGc.reuseCount.get() >= (N/2)) // the exact count is non-deterministic
  }

  it should "copy itself to a List" in {
    val s = IBRStackDebug[Int]()
    val tc = IBRStackDebug.threadLocalContext[Int]()
    s.push(1, tc)
    s.push(2, tc)
    s.push(3, tc)
    assert(s.unsafeToList(tc) === List(3, 2, 1))
  }
}
