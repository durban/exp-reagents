/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
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

package io.sigs.choam
package bench
package util

import java.util.concurrent.ThreadLocalRandom

import scala.concurrent.stm._

import cats.effect.IO

import fs2.async

class StmStackSpec extends BaseSpec {

  "StmStack" should "be a correct stack" in {
    val s = new StmStack[Int]
    s.unsafeToList() should === (Nil)
    s.tryPop() should === (None)
    s.unsafeToList() should === (Nil)
    s.push(1)
    s.unsafeToList() should === (1 :: Nil)
    s.push(2)
    s.push(3)
    s.unsafeToList() should === (3 :: 2 :: 1 :: Nil)
    s.tryPop() should === (Some(3))
    s.unsafeToList() should === (2 :: 1 :: Nil)
    s.tryPop() should === (Some(2))
    s.tryPop() should === (Some(1))
    s.tryPop() should === (None)
    s.unsafeToList() should === (Nil)
  }

  it should "not lose items" in {
    val s = new StmStack[Int]
    val N = 1000000
    val seed1 = ThreadLocalRandom.current().nextInt()
    val seed2 = ThreadLocalRandom.current().nextInt()
    def push(xs: XorShift): Unit = {
      for (_ <- 1 to N) {
        s.push(xs.nextInt())
      }
    }
    @tailrec
    def pop(i: Int, cs: Int = 0): Int = {
      if (i > 0) {
        s.tryPop() match {
          case Some(item) =>
            pop(i - 1, cs ^ item)
          case None =>
            pop(i, cs)
        }
      } else {
        cs
      }
    }

    val tsk = for {
      fpu1 <- async.start(IO { push(XorShift(seed1)) })
      fpu2 <- async.start(IO { push(XorShift(seed2)) })
      fpo1 <- async.start(IO { pop(N) })
      fpo2 <- async.start(IO { pop(N) })
      _ <- fpu1
      _ <- fpu2
      cs1 <- fpo1
      cs2 <- fpo2
    } yield cs1 ^ cs2

    val cs = tsk.unsafeRunSync()
    val xs1 = XorShift(seed1)
    val expCs1 = (1 to N).foldLeft(0) { (cs, _) =>
      cs ^ xs1.nextInt()
    }
    val xs2 = XorShift(seed2)
    val expCs2 = (1 to N).foldLeft(0) { (cs, _) =>
      cs ^ xs2.nextInt()
    }
    cs should === (expCs1 ^ expCs2)
    s.unsafeToList() should === (Nil)
  }

  it should "have composable transactions" in {
    val s1 = new StmStack[Int]
    val s2 = new StmStack[Int]
    val N = 1000000
    def push(xs: XorShift): Unit = {
      for (_ <- 1 to N) {
        val item = xs.nextInt()
        atomic { implicit txn =>
          s1.push(item)
          s2.push(item)
        }
      }
    }
    def pop(): Unit = {
      for (_ <- 1 to N) {
        atomic { implicit txn =>
          val i1 = s1.tryPop()
          val i2 = s2.tryPop()
          (i1, i2) match {
            case (Some(v1), Some(v2)) =>
              if (v1 !== v2) fail(s"Popped different values: ${v1} and ${v2}")
            case (None, None) =>
              // OK, empty stacks
            case _ =>
              fail(s"Popped different items: ${i1} and ${i2}")
          }
        }
      }
    }
    val tsk = for {
      fpu1 <- async.start(IO { push(XorShift()) })
      fpo1 <- async.start(IO { pop() })
      fpu2 <- async.start(IO { push(XorShift()) })
      fpo2 <- async.start(IO { pop() })
      _ <- fpu1
      _ <- fpu2
      _ <- fpo1
      _ <- fpo2
    } yield ()

    tsk.unsafeRunSync()
  }
}
