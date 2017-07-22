package io.sigs.choam

import org.openjdk.jcstress.annotations.{ Actor, Arbiter, Outcome }
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LL_Result

import kcas._

@KCASParams("Ref#getter should be consistent")
@Outcomes(Array(
  new Outcome(id = Array("(foo,bar), (x,y)"), expect = ACCEPTABLE, desc = "Read old values"),
  new Outcome(id = Array("(x,y), (x,y)"), expect = ACCEPTABLE, desc = "Read new values"),
))
abstract class ConsistentReadTest(impl: KCAS) {

  private[this] var ref1 =
    Ref.mk("foo")

  private[this] var ref2 =
    Ref.mk("bar")

  private[this] var upd: React[Unit, Unit] =
    ref1.cas("foo", "x") >>> ref2.cas("bar", "y")

  private[this] var get: React[Unit, (String, String)] =
    ref1.getter * ref2.getter

  @Actor
  def update(): Unit = {
    upd.unsafeRun
  }

  @Actor
  def read(r: LL_Result): Unit = {
    r.r1 = get.unsafeRun
  }

  @Arbiter
  def arbiter(r: LL_Result): Unit = {
    r.r2 = (ref1.getter.unsafeRun, ref2.getter.unsafeRun)
  }
}
