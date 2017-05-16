package com.example.rea
package bench
package util

import java.util.concurrent.ThreadLocalRandom

import scala.concurrent.stm._

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
      fpu1 <- Task.start(Task.delay { push(XorShift(seed1)) })
      fpu2 <- Task.start(Task.delay { push(XorShift(seed2)) })
      fpo1 <- Task.start(Task.delay { pop(N) })
      fpo2 <- Task.start(Task.delay { pop(N) })
      _ <- fpu1
      _ <- fpu2
      cs1 <- fpo1
      cs2 <- fpo2
    } yield cs1 ^ cs2

    val cs = tsk.unsafeRun()
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
      fpu1 <- Task.start(Task.delay { push(XorShift()) })
      fpo1 <- Task.start(Task.delay { pop() })
      fpu2 <- Task.start(Task.delay { push(XorShift()) })
      fpo2 <- Task.start(Task.delay { pop() })
      _ <- fpu1
      _ <- fpu2
      _ <- fpo1
      _ <- fpo2
    } yield ()

    tsk.unsafeRun()
  }
}
