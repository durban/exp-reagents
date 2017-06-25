package io.sigs.choam
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.ZZL_Result

@KCASParams("CAS1 should be atomic")
@Outcomes(Array(
  new Outcome(id = Array("true, false, x"), expect = ACCEPTABLE, desc = "T1 succeeded"),
  new Outcome(id = Array("false, true, y"), expect = ACCEPTABLE, desc = "T2 succeeded"),
))
abstract class CAS1Test(impl: KCAS) {

  private[this] val ref: Ref[String] =
    Ref.mk("ov")

  @Actor
  def writer1(r: ZZL_Result): Unit = {
    r.r1 = impl.start().withCAS(ref, "ov", "x").tryPerform()
  }

  @Actor
  def writer2(r: ZZL_Result): Unit = {
    r.r2 = impl.start().withCAS(ref, "ov", "y").tryPerform()
  }

  @Actor
  def reader(r: ZZL_Result): Unit = {
    r.r3 = impl.read(ref)
  }

  @Arbiter
  def arbiter(r: ZZL_Result): Unit = {
    val fv = impl.read(ref)
    r.r3 match {
      case null =>
        throw new AssertionError(s"unexpected value: null")
      case "ov" =>
        // OK
      case nv if (nv eq fv) =>
        // OK
      case nv =>
        throw new AssertionError(s"unexpected value: ${nv}")
    }
    r.r3 = fv
  }
}
