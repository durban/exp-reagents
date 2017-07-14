package io.sigs.choam

import org.openjdk.jcstress.annotations.{ Actor, Arbiter, Outcome }
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LLLLL_Result

import kcas._

@KCASParams("Changes by the reaction must be visible in post-commit actions")
@Outcomes(Array(
  new Outcome(id = Array("(foo,bar), x, (x,x), y, (y,y)"), expect = ACCEPTABLE, desc = "u1 first, pc reads result"),
  new Outcome(id = Array("(foo,bar), y, (x,x), y, (y,y)"), expect = ACCEPTABLE, desc = "u1 first, pc reads u2 result"),
  new Outcome(id = Array("(y,y), x, (foo,bar), y, (x,x)"), expect = ACCEPTABLE, desc = "u2 first, pc reads result"),
  new Outcome(id = Array("(y,y), x, (foo,bar), x, (x,x)"), expect = ACCEPTABLE, desc = "u2 first, pc reads u1 result"),
))
abstract class PostCommitTest(impl: KCAS) {

  protected implicit final val kcasImpl: KCAS =
    impl

  private[this] val r1 =
    Ref.mk("foo")

  private[this] val r2 =
    Ref.mk("bar")

  private[this] val upd: React[String, (String, String)] =
    r1.upd[String, String] { (ov, nv) => (nv, ov) } * r2.upd[String, String] { (ov, nv) => (nv, ov) }

  @Actor
  def upd1(r: LLLLL_Result): Unit = {
    val u1 = upd.postCommit(React.lift[(String, String), Unit] { res =>
      r.r1 = res
      r.r2 = r1.getter.unsafeRun
      ()
    })
    u1.unsafePerform("x")
  }

  @Actor
  def upd2(r: LLLLL_Result): Unit = {
    val u2 = upd.postCommit(React.lift[(String, String), Unit] { res =>
      r.r3 = res
      r.r4 = r1.getter.unsafeRun
      ()
    })
    u2.unsafePerform("y")
  }

  @Arbiter
  def arbiter(r: LLLLL_Result): Unit = {
    r.r5 = (r1.getter.unsafeRun, r2.getter.unsafeRun)
  }
}
