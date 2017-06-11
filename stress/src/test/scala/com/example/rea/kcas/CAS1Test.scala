package com.example.rea
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.infra.results.ZZL_Result

@JCStressTest
@Description("CAS1 should be atomic")
@State
@Outcomes(Array(
  new Outcome(id = Array("true, false, x"), expect = ACCEPTABLE, desc = "T1 succeeded"),
  new Outcome(id = Array("false, true, y"), expect = ACCEPTABLE, desc = "T2 succeeded")
))
class CAS1Test {

  private[this] val ref =
    Ref.mk("ov")

  private[this] val impl: KCAS =
    MCAS // TODO: test others

  @Actor
  def actor1(r: ZZL_Result): Unit = {
    r.r1 = impl.start().withCAS(ref, "ov", "x").tryPerform()
  }

  @Actor
  def actor2(r: ZZL_Result): Unit = {
    r.r2 = impl.start().withCAS(ref, "ov", "y").tryPerform()
  }

  @Arbiter
  def arbiter(r: ZZL_Result): Unit = {
    r.r3 = impl.read(ref)
  }
}
