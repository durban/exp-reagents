package com.example.rea
package bench
package util

import java.util.concurrent.ThreadLocalRandom

import scala.concurrent.stm._

import fs2._

import org.scalatest.{ FlatSpec, Matchers }
import org.scalactic.TypeCheckedTripleEquals

class StmQueueSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  implicit val str: Strategy =
    Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.global)

  "StmQueue" should "be a correct queue" in {
    val q = new StmQueue[Int]
    q.unsafeToList() should === (Nil)
    q.tryDequeue() should === (None)
    q.enqueue(1)
    q.unsafeToList() should === (1 :: Nil)
    q.enqueue(2)
    q.enqueue(3)
    q.unsafeToList() should === (1 :: 2 :: 3 :: Nil)
    q.tryDequeue() should === (Some(1))
    q.unsafeToList() should === (2 :: 3 :: Nil)
    q.tryDequeue() should === (Some(2))
    q.unsafeToList() should === (3 :: Nil)
    q.enqueue(9)
    q.unsafeToList() should === (3 :: 9 :: Nil)
    q.tryDequeue() should === (Some(3))
    q.unsafeToList() should === (9 :: Nil)
    q.tryDequeue() should === (Some(9))
    q.unsafeToList() should === (Nil)
    q.tryDequeue() should === (None)
    q.unsafeToList() should === (Nil)
    q.tryDequeue() should === (None)
  }

  it should "not lose items" in {
    val q = new StmQueue[Int]
    val N = 1000000
    def enq(xs: XorShift): Unit = {
      for (_ <- 1 to N) {
        q.enqueue(xs.nextInt())
      }
    }
    def deq(i: Int, cs: Int = 0): Int = {
      if (i > 0) {
        q.tryDequeue() match {
          case Some(item) =>
            deq(i - 1, cs ^ item)
          case None =>
            deq(i, cs)
        }
      } else {
        cs
      }
    }

    val seed1 = ThreadLocalRandom.current().nextInt()
    val seed2 = ThreadLocalRandom.current().nextInt()
    val tsk = for {
      fpu1 <- Task.start(Task.delay { enq(XorShift(seed1)) })
      fpu2 <- Task.start(Task.delay { enq(XorShift(seed2)) })
      fpo1 <- Task.start(Task.delay { deq(N) })
      fpo2 <- Task.start(Task.delay { deq(N) })
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
    q.unsafeToList() should === (Nil)
  }

  it should "have composable transactions" in {
    val q1 = new StmQueue[Int]
    val q2 = new StmQueue[Int]
    val N = 1000000
    def enq(xs: XorShift): Unit = {
      for (_ <- 1 to N) {
        val item = xs.nextInt()
        atomic { implicit txn =>
          q1.enqueue(item)
          q2.enqueue(item)
        }
      }
    }
    def deq(): Unit = {
      for (_ <- 1 to N) {
        atomic { implicit txn =>
          val i1 = q1.tryDequeue()
          val i2 = q2.tryDequeue()
          (i1, i2) match {
            case (Some(v1), Some(v2)) =>
              if (v1 !== v2) fail(s"Dequeued different values: ${v1} and ${v2}")
            case (None, None) =>
              // OK, empty queues
            case _ =>
              fail(s"Dequeued different items: ${i1} and ${i2}")
          }
        }
      }
    }
    val tsk = for {
      fpu1 <- Task.start(Task.delay { enq(XorShift()) })
      fpo1 <- Task.start(Task.delay { deq() })
      fpu2 <- Task.start(Task.delay { enq(XorShift()) })
      fpo2 <- Task.start(Task.delay { deq() })
      _ <- fpu1
      _ <- fpu2
      _ <- fpo1
      _ <- fpo2
    } yield ()

    tsk.unsafeRun()
  }
}
