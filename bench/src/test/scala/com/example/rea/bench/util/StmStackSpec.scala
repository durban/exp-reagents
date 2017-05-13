package com.example.rea
package bench
package util

import fs2._

import org.scalatest.{ FlatSpec, Matchers }
import org.scalactic.TypeCheckedTripleEquals

class StmStackSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  implicit val str: Strategy =
    Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.global)

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
    def push(range: Range): Unit = {
      for (i <- range) {
        s.push(3 * i + 42)
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
      fpu1 <- Task.start(Task.delay { push(1 to N) })
      fpu2 <- Task.start(Task.delay { push((N + 1) to (2 * N)) })
      fpo1 <- Task.start(Task.delay { pop(N) })
      fpo2 <- Task.start(Task.delay { pop(N) })
      _ <- fpu1
      _ <- fpu2
      cs1 <- fpo1
      cs2 <- fpo2
    } yield cs1 ^ cs2

    val cs = tsk.unsafeRun()
    val expCs = (1 to 2 * N).foldLeft(0) { (cs, i) =>
      cs ^ (3 * i + 42)
    }
    cs should === (expCs)
    s.unsafeToList() should === (Nil)
  }
}
