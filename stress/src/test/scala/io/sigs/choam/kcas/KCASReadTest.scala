package io.sigs.choam
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.ZL_Result

@KCASParams("k-CAS should be atomic to readers")
@Outcomes(Array(
  new Outcome(id = Array("true, Set(ov)"), expect = ACCEPTABLE, desc = "Read old values"),
  new Outcome(id = Array("true, Set(x)"), expect = ACCEPTABLE, desc = "Read new values"),
  new Outcome(id = Array("true, Set(ov, x)", "true, Set(x, ov)"), expect = ACCEPTABLE, desc = "Read both values"),
))
abstract class KCASReadTest(impl: KCAS) {

  private[this] val refs: List[Ref[String]] =
    List.fill(8)(Ref.mk("ov"))

  @Actor
  def writer(r: ZL_Result): Unit = {
    val d = refs.foldLeft(impl.start()) { (d, ref) =>
      d.withCAS(ref, "ov", "x")
    }
    r.r1 = d.tryPerform()
  }

  @Actor
  def reader(r: ZL_Result): Unit = {
    r.r2 = refs.map(impl.read(_)).toSet
  }
}
