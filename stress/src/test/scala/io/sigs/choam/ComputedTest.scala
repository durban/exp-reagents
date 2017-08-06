package io.sigs.choam

import org.openjdk.jcstress.annotations.{ Actor, Arbiter, Outcome }
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LLL_Result

import kcas._

@KCASParams("Computed reagents should be executed atomically")
@Outcomes(Array(
  new Outcome(id = Array("www, (foo,bar), (www,y)"), expect = ACCEPTABLE, desc = "Writer runs first; reader sees old values"),
  new Outcome(id = Array("www, (www,y), (www,y)"), expect = ACCEPTABLE, desc = "Writer runs first; reader sees new values"),
  new Outcome(id = Array("www, (www,bar), (www,y)"), expect = ACCEPTABLE, desc = "Writer runs first; reader sees new/old values"),
  new Outcome(id = Array("foo, (foo,bar), (www,x)"), expect = ACCEPTABLE, desc = "Computed runs first; reader sees old values"),
  new Outcome(id = Array("foo, (www,x), (www,x)"), expect = ACCEPTABLE, desc = "Computed runs first; reader sees new values"),
  new Outcome(id = Array("foo, (www,bar), (www,x)"), expect = ACCEPTABLE, desc = "Computed runs first; reader sees new/old values"),
  new Outcome(id = Array("foo, (foo,x), (www,x)"), expect = ACCEPTABLE, desc = "Computed runs first; reader sees old/new values"),
))
abstract class ComputedTest(impl: KCAS) {

  private[this] val r1 =
    Ref.mk("foo")

  private[this] val r2 =
    Ref.mk("bar")

  private[this] val write =
    r1.upd[String, String] { (ov, nv) => (nv, ov) }

  private[this] val w1 =
    r2.upd[Unit, String] { (ov, _) => ("x", ov) }

  private[this] val w2 =
    r2.upd[Unit, String] { (ov, _) => ("y", ov) }

  private[this] val computed: React[Unit, String] = {
    React.invisibleRead(r1) >>> React.computed[String, String] { a =>
      val w = if (a eq "foo") w1 else w2
      (w * React.cas(r1, a, a)).map { _ => a }
    }
  }

  private[this] val consistentRead: React[Unit, (String, String)] =
    React.consistentRead(r1, r2)

  @Actor
  def writer(): Unit = {
    write.unsafePerform("www")
  }

  @Actor
  def computer(r: LLL_Result): Unit = {
    r.r1 = computed.unsafeRun
  }

  @Actor
  def reader(r: LLL_Result): Unit = {
    r.r2 = consistentRead.unsafeRun
  }

  @Arbiter
  def arbiter(r: LLL_Result): Unit = {
    r.r3 = (r1.getter.unsafeRun, r2.getter.unsafeRun)
  }
}
