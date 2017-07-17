package io.sigs.choam
package macros

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.ZZZ_Result

import kcas.KCAS

@KCASParams("Dummy test", true)
@Outcomes(Array(
  new Outcome(id = Array("true, true, true"), expect = ACCEPTABLE, desc = "OK"),
  new Outcome(id = Array("false, true, true"), expect = ACCEPTABLE, desc = "OK too"),
))
abstract class DummyTest(impl: KCAS) {

  final def kcasImplPublic = kcasImpl

  def foo(i: Int): Unit = {
    println(i)
  }

  @Actor
  def actor1(r: ZZZ_Result): Unit = {
    r.r1 = impl.## > 0
  }

  @Actor
  def actor2(r: ZZZ_Result): Unit = {
    r.r2 = true
  }

  @Arbiter
  def arbiter(r: ZZZ_Result): Unit = {
    r.r3 = r.r1 || r.r2
  }
}
