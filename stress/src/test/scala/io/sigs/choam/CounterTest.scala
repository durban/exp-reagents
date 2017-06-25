package io.sigs.choam

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.JJJJ_Result

import kcas._

@KCASParams("Counter incr/decr/count should be atomic")
@Outcomes(Array(
  new Outcome(id = Array("0, 1, 0, 0", "0, 1, 1, 0"), expect = ACCEPTABLE, desc = "incr is first"),
  new Outcome(id = Array("-1, 0, 0, 0", "-1, 0, -1, 0"), expect = ACCEPTABLE, desc = "decr is first"),
))
abstract class CounterTest(impl: KCAS) {

  protected implicit final val kcasImpl: KCAS =
    impl

  private[this] val ctr =
    new Counter()

  private[this] val incr =
    ctr.incr

  private[this] val decr =
    ctr.decr

  private[this] val count =
    ctr.count

  @Actor
  def increment(r: JJJJ_Result): Unit = {
    r.r1 = incr.unsafePerform(())
  }

  @Actor
  def decrement(r: JJJJ_Result): Unit = {
    r.r2 = decr.unsafePerform(())
  }

  @Actor
  def value(r: JJJJ_Result): Unit = {
    r.r3 = count.unsafePerform(())
  }

  @Arbiter
  def arbiter(r: JJJJ_Result): Unit = {
    r.r4 = count.unsafePerform(())
  }
}
