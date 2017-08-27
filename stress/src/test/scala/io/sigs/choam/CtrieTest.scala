package io.sigs.choam

import cats.Eq
import cats.instances.string.catsKernelStdOrderForString

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LL_Result

import kcas.KCAS

@KCASParams("Ctrie insert/lookup should be atomic") // TODO: even when composed
@Outcomes(Array(
  new Outcome(id = Array("None, Some(1)", "None, Some(2)"), expect = ACCEPTABLE, desc = "get first"),
  new Outcome(id = Array("Some(1), Some(2)"), expect = ACCEPTABLE, desc = "ins1, get, ins2"),
  new Outcome(id = Array("Some(2), Some(1)"), expect = ACCEPTABLE, desc = "ins2, get, ins1"),
  new Outcome(id = Array("Some(1), Some(1)", "Some(2), Some(2)"), expect = ACCEPTABLE, desc = "get last"),
))
abstract class CtrieTest(impl: KCAS) {

  private[this] val ctrie =
    new Ctrie[String, String](_.##, Eq[String])

  private[this] val insert =
    ctrie.insert

  private[this] val lookup =
    ctrie.lookup

  @Actor
  def ins1(): Unit = {
    insert.unsafePerform("x" -> "1")
  }

  @Actor
  def ins2(): Unit = {
    insert.unsafePerform("x" -> "2")
  }

  @Actor
  def get(r: LL_Result): Unit = {
    r.r1 = lookup.unsafePerform("x")
  }

  @Arbiter
  def arbiter(r: LL_Result): Unit = {
    r.r2 = lookup.unsafePerform("x")
  }
}
