package com.example.rea
package kcas

import java.util.concurrent.atomic.AtomicInteger
import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.infra.results.II_Result

@JCStressTest
@Description("Example test")
@State
@Outcomes(Array(
  new Outcome(id = Array("1, 2"), expect = ACCEPTABLE, desc = "T1 then T2"),
  new Outcome(id = Array("2, 1"), expect = ACCEPTABLE, desc = "T2 then T1")
))
class StressTest {

  private[this] val v =
    new AtomicInteger(0)

  @Actor
  def actor1(r: II_Result): Unit = {
    r.r1 = v.incrementAndGet()
  }

  @Actor
  def actor2(r: II_Result): Unit = {
    r.r2 = v.incrementAndGet()
  }
}
