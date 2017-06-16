package com.example.rea
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.ZZL_Result

@KCASParams("k-CAS should be atomic")
@Outcomes(Array(
  new Outcome(id = Array("true, false, x"), expect = ACCEPTABLE, desc = "T1 succeeded"),
  new Outcome(id = Array("false, true, y"), expect = ACCEPTABLE, desc = "T2 succeeded"),
))
abstract class KCASTest(impl: KCAS) {

  private[this] val refs: List[Ref[String]] =
    List.fill(8)(Ref.mk("ov"))

  private def write(nv: String): Boolean = {
    val d = refs.foldLeft(impl.start()) { (d, ref) =>
      d.withCAS(ref, "ov", nv)
    }
    d.tryPerform()
  }

  @Actor
  def writer1(r: ZZL_Result): Unit = {
    r.r1 = write("x")
  }

  @Actor
  def writer2(r: ZZL_Result): Unit = {
    r.r2 = write("y")
  }

  @Arbiter
  def arbiter(r: ZZL_Result): Unit = {
    val vs = refs.map(impl.read(_))
    val s = vs.toSet
    if (s.size == 1) {
      r.r3 = s.iterator.next()
    } else {
      throw new AssertionError(s"invalid values: ${s}")
    }
  }
}
