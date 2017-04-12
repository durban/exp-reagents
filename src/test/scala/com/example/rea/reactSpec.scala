package com.example.rea

import java.util.concurrent.LinkedBlockingDeque

import scala.collection.JavaConverters._

import cats.Eq
import cats.laws.discipline.ArrowTests
import cats.implicits._

import org.scalacheck.{ Gen, Arbitrary }

import org.scalatest.{ FlatSpec, Matchers, FunSuite }
import org.scalactic.TypeCheckedTripleEquals

import org.typelevel.discipline.scalatest.Discipline

import fs2.{ Task, Strategy }
import com.example.rea.kcas.Ref

class ReactSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  import React._

  implicit val str: Strategy =
    Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.global)

  "Simple CAS" should "work as expected" in {
    val ref = Ref.mk("ert")
    val rea = lift((_: Int).toString) × (ref.cas("ert", "xyz") >>> lift(_ => "boo"))
    val (s1, s2) = rea ! ((5, ()))
    s1 should === ("5")
    s2 should === ("boo")
    ref.read.run should === ("xyz")
  }

  "Combined updates" should "indeed be atomic" in {
    val r1 = Ref.mk[List[Int]](Nil)
    val r2 = Ref.mk[List[Int]](Nil)

    def pushBoth(i: Int): Unit = {
      def push(r: Ref[List[Int]], i: Int): React[Unit, Unit] = {
        r.upd[Unit, Unit] { (l, _) =>
          (i :: l, ())
        }
      }
      val r = push(r1, i) * push(r2, i)
      r.run
    }

    def pushAll(maxSize: Long): Unit = {
      for (_ <- 1L to maxSize) {
        val i = java.util.concurrent.ThreadLocalRandom.current().nextInt()
        pushBoth(i)
      }
    }

    val n = 80000L
    val m = 50000L
    val tsk = for {
      f1a <- Task.start(Task.delay { pushAll(n) })
      f1b <- Task.start(Task.delay { pushAll(n) })
      f2 <- Task.start(Task.delay { pushAll(m) })
      _ <- f1a
      _ <- f1b
      _ <- f2
    } yield ()
    tsk.unsafeRun()

    val l1 = r1.read.run
    val l2 = r2.read.run
    l1.length.toLong should === (2 * n + m)
    l2.length.toLong should === (2 * n + m)
    for ((i1, i2) <- l1 zip l2) {
      i1 should === (i2)
    }
  }

  def pushAll(r: React[Int, _], count: Int): Unit = {
    for (_ <- 1 to count) {
      val i = java.util.concurrent.ThreadLocalRandom.current().nextInt()
      r ! i
    }
  }

  def popAll(r: React[Unit, List[Int]], expLen: Int, count: Int, errors: LinkedBlockingDeque[String]): Unit = {
    require(expLen > 0)
    for (_ <- 1 to count) {
      val i = java.util.concurrent.ThreadLocalRandom.current().nextInt()
      val lst = r.run
      if (lst.length =!= expLen) {
        if (lst.length =!= 0) {
          errors.offer(s"actual length ${lst.length} doesn't equal expected length ${expLen}")
        } // else: OK, drained the stacks
      } else if (!lst.forall(_ == lst.head)) {
        errors.offer(s"not all popped items are equal")
      }
    }
  }

  "2 combined stack" should "work atomically" in {
    val s1 = new TreiberStack[Int]
    val s2 = new TreiberStack[Int]
    val push = s1.push * s2.push
    val pop = s1.tryPop * s2.tryPop
    val errors = new java.util.concurrent.LinkedBlockingDeque[String](100)
    val n = 8000000
    val m = 7000000
    val tsk = for {
      push1 <- Task.start(Task.delay { pushAll(push.rmap(_ => ()), n) })
      push2 <- Task.start(Task.delay { pushAll(push, n) })
      pop1 <- Task.start(Task.delay { popAll(pop.rmap { case (o1, o2) => o1.toList ++ o2.toList }, expLen = 2, count = m, errors = errors) })
      pop2 <- Task.start(Task.delay { popAll(pop.rmap { case (o1, o2) => o1.toList ++ o2.toList }, expLen = 2, count = m, errors = errors) })
      _ <- push1
      _ <- push2
      _ <- pop1
      _ <- pop2
    } yield ()
    tsk.unsafeRun()

    if (!errors.isEmpty) {
      fail(s"Errors:\n${errors.asScala.mkString("\n")}")
    }

    val l1 = s1.head.read.run
    val l2 = s2.head.read.run
    if (l1 != l2) {
      fail("Different stacks at the end")
    }
  }

  // TODO: figure out how could we actually deadlock
  "Deadlocks" should "not be possible" ignore {
    val nStacks = 10
    val stacks = List.fill(nStacks)(new TreiberStack[Int])
    val push = stacks.map(_.push).reduceLeft[React[Int, Unit]]{ (a, b) => (a * b).rmap(_ => ()) }
    val pushFlipped = stacks.reverse.map(_.push).reduceLeft[React[Int, Unit]] { (a, b) => (a * b).rmap(_ => ()) }
    val pop = stacks.map(_.tryPop.rmap(_.toList)).reduceLeft[React[Unit, List[Int]]] { (a, b) => (a * b).rmap { case (x, y) => x ++ y } }
    val popFlipped = stacks.reverse.map(_.tryPop.rmap(_.toList)).reduceLeft[React[Unit, List[Int]]] { (a, b) => (a * b).rmap { case (x, y) => x ++ y } }
    val errors = new java.util.concurrent.LinkedBlockingDeque[String](100)
    val n = 8000000
    val m = 7500000
    val tsk = for {
      push1 <- Task.start(Task.delay { pushAll(push, n) })
      push2 <- Task.start(Task.delay { pushAll(pushFlipped, n) })
      pop1 <- Task.start(Task.delay { popAll(pop, expLen = nStacks, count = m, errors) })
      pop2 <- Task.start(Task.delay { popAll(popFlipped, expLen = nStacks, count = m, errors) })
      _ <- push1
      _ <- push2
      _ <- pop1
      _ <- pop2
    } yield ()
    tsk.unsafeRun()

    if (!errors.isEmpty) {
      fail(s"Errors:\n${errors.asScala.mkString("\n")}")
    }

    val lsts = stacks.map(_.head.read.run)
    if (!lsts.forall(lst => lst == lsts.head)) {
      fail("Different stacks at the end")
    }
  }

  "Choice" should "prefer the first option" in {
    val r1 = Ref.mk("r1")
    val r2 = Ref.mk("r2")
    val rea = r1.cas("r1", "x") + r2.cas("r2", "x")
    val res = rea ! (())
    res should === (())
    r1.read.run should === ("x")
    r2.read.run should === ("r2")
  }

  it should "use the second option, if the first is not available" in {
    val r1 = Ref.mk("z")
    val r2 = Ref.mk("r2")
    val rea = r1.cas("r1", "x") + (r2.cas("r2", "x") * r1.cas("z", "r1"))
    // r2: "r2" -> "x" AND r1: "z" -> "r1"
    rea.run
    r2.read.run should === ("x")
    r1.read.run should === ("r1")
    // r1: "r1" -> "x"
    rea.run
    r1.read.run should === ("x")
    r2.read.run should === ("x")
  }

  "Popping then pushing back" should "work (???)" in {
    val stack = new TreiberStack[Int]
    val popPush = stack.tryPop.rmap(_.getOrElse(0)) >>> stack.push

    stack.unsafeToList should === (List())
    stack.push ! 1
    stack.unsafeToList should === (List(1))

    // FIXME:
    popPush.run
    stack.unsafeToList should === (List(1, 1))
  }

  "Impossible CAS" should "work (???)" in {
    val ref = Ref.mk("foo")
    val cas1 = ref.cas("foo", "bar")
    val cas2 = ref.cas("foo", "baz")
    val r = cas1 >>> cas2
    r.run

    // FIXME:
    ref.read.run should === ("baz")
  }
}

class LawsSpec extends FunSuite with Discipline {

  implicit def arbReact[A, B](implicit arbA: Arbitrary[A], arbB: Arbitrary[B], arbAB: Arbitrary[A => B]): Arbitrary[React[A, B]] = Arbitrary {
    Gen.oneOf(
      arbAB.arbitrary.map(ab => React.lift[A, B](ab)),
      arbAB.arbitrary.map(ab => React.identity[A] >>> React.lift[A, B](ab)),
      Gen.lzy {
        for {
          one <- arbReact[A, B].arbitrary
          two <- arbReact[A, B].arbitrary
        } yield one + two
      },
      Gen.lzy {
        arbB.arbitrary.map { b =>
          val ref = Ref.mk(b)
          ref.read.lmap[A](_ => ())
        }
      },
      Gen.lzy {
        for {
          ab <- arbAB.arbitrary
          a1 <- arbA.arbitrary
          a2 <- arbA.arbitrary
        } yield {
          val r = React.newRef(a1).first[A].flatMap { case (ref, _) =>
            ref.upd[(Unit, A), B] { (a1, a2) => (a2._2, ab(a1)) }
          }
          r.lmap[A](a => ((), a))
        }
      },
      arbAB.arbitrary.map { fab =>
        val s = "x"
        val ref = Ref.mk(s)
        (React.lift[A, B](fab) × ref.cas(s, s)).lmap[A](a => (a, ())).rmap(_._1)
      }
    )
  }

  implicit def sketchyEq[A, B](implicit arbA: Arbitrary[A], equB: Eq[B]): Eq[React[A, B]] = new Eq[React[A, B]] {
    def eqv(x: React[A, B], y: React[A, B]): Boolean = {
      (1 to 1000).forall { _ =>
        val a = arbA.arbitrary.sample.getOrElse(fail)
        val bx = x ! a
        val by = y ! a
        equB.eqv(bx, by)
      }
    }
  }

  checkAll("Arrow[React]", ArrowTests[React].arrow[Int, Int, Int, Int, Int, Int])
}
