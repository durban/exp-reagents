package io.sigs.choam

import cats.Eq
import cats.laws.discipline.ArrowTests
import cats.implicits._

import org.scalacheck.{ Gen, Arbitrary }

import org.scalatest.FunSuite

import org.typelevel.discipline.scalatest.Discipline

import kcas._

class LawsSpecNaiveKCAS
  extends LawsSpec
  with SpecNaiveKCAS

class LawsSpecCASN
  extends LawsSpec
  with SpecCASN

class LawsSpecMCAS
  extends LawsSpec
  with SpecMCAS

abstract class LawsSpec extends FunSuite with Discipline with KCASImplSpec {

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
      },
      Gen.lzy {
        for {
          r <- arbReact[A, B].arbitrary
          b <- arbB.arbitrary
        } yield {
          val ref = Ref.mk[B](b)
          r.postCommit(ref.upd[B, Unit] { case (_, b) => (b, ()) })
        }
      }
    )
  }

  implicit def sketchyEq[A, B](implicit arbA: Arbitrary[A], equB: Eq[B]): Eq[React[A, B]] = new Eq[React[A, B]] {
    def eqv(x: React[A, B], y: React[A, B]): Boolean = {
      (1 to 1000).forall { _ =>
        val a = arbA.arbitrary.sample.getOrElse(fail)
        val bx = x.unsafePerform(a)
        val by = y.unsafePerform(a)
        equB.eqv(bx, by)
      }
    }
  }

  checkAll("Arrow[React]", ArrowTests[React].arrow[Int, Int, Int, Int, Int, Int])
}
