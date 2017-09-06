package io.sigs.choam

import cats.Eq

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LL_Result

import kcas.KCAS

@KCASParams("Ctrie insert/lookup should be atomic") // TODO: even when composed
@Outcomes(Array(
  new Outcome(id = Array("Some(0), Some(x)", "Some(0), Some(y)"), expect = ACCEPTABLE, desc = "get first"),
  new Outcome(id = Array("Some(x), Some(y)"), expect = ACCEPTABLE, desc = "ins1, get, ins2"),
  new Outcome(id = Array("Some(y), Some(x)"), expect = ACCEPTABLE, desc = "ins2, get, ins1"),
  new Outcome(id = Array("Some(x), Some(x)", "Some(y), Some(y)"), expect = ACCEPTABLE, desc = "get last"),
))
abstract class CtrieTest(impl: KCAS) {

  private[this] val ctrie = {
    val ct = new Ctrie[Int, String](_ % 7, Eq.instance(_ % 14 == _ % 14))
    ct.insert.unsafePerform(0 -> "0")(impl)
    ct.insert.unsafePerform(1 -> "1")(impl)
    ct.insert.unsafePerform(2 -> "2")(impl)
    ct.insert.unsafePerform(3 -> "3")(impl)
    ct.insert.unsafePerform(4 -> "4")(impl)
    ct.insert.unsafePerform(7 -> "7")(impl)
    ct.insert.unsafePerform(8 -> "8")(impl)
    ct.insert.unsafePerform(9 -> "9")(impl)
    ct
  }

  private[this] val insert =
    ctrie.insert

  private[this] val lookup =
    ctrie.lookup

  @Actor
  def ins1(): Unit = {
    insert.unsafePerform(14 -> "x")
  }

  @Actor
  def ins2(): Unit = {
    insert.unsafePerform(14 -> "y")
  }

  @Actor
  def get(r: LL_Result): Unit = {
    r.r1 = lookup.unsafePerform(0)
  }

  @Arbiter
  def arbiter(r: LL_Result): Unit = {
    r.r2 = lookup.unsafePerform(0)
    assert(lookup.unsafePerform(1).get eq "1")
    assert(lookup.unsafePerform(2).get eq "2")
    assert(lookup.unsafePerform(3).get eq "3")
    assert(lookup.unsafePerform(4).get eq "4")
    assert(lookup.unsafePerform(7).get eq "7")
    assert(lookup.unsafePerform(8).get eq "8")
    assert(lookup.unsafePerform(9).get eq "9")
  }
}
