package io.sigs.choam

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LL_Result

import kcas.KCAS

// TODO: enable this test, and fix the failure!
@KCASParams("Composed Ctrie insert/lookup should be atomic", true)
@Outcomes(Array(
  new Outcome(id = Array("(Some(0),Some(1)), (Some(x),Some(y))"), expect = ACCEPTABLE, desc = "get first"),
  new Outcome(id = Array("(Some(x),Some(y)), (Some(x),Some(y))"), expect = ACCEPTABLE, desc = "ins first"),
))
abstract class CtrieComposedTest(impl: KCAS) {

  private[this] val ct1 =
    CtrieTest.newCtrie714()

  private[this] val ct2 =
    CtrieTest.newCtrie714()

  private[this] val insert: React[((Int, String), (Int, String)), (Unit, Unit)] =
    ct1.insert × ct2.insert

  private[this] val lookup: React[(Int, Int), (Option[String], Option[String])] =
    ct1.lookup × ct2.lookup

  @Actor
  def ins(): Unit = {
    insert.unsafePerform((14 -> "x", 1 -> "y"))
    ()
  }

  @Actor
  def get(r: LL_Result): Unit = {
    r.r1 = lookup.unsafePerform((14, 1))
  }

  @Arbiter
  def arbiter(r: LL_Result): Unit = {
    r.r2 = lookup.unsafePerform((14, 1))
  }
}
