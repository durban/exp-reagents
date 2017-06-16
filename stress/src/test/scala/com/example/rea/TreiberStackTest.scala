package com.example.rea

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LL_Result

import kcas._

@KCASParams("Treiber stack pop/push should be atomic")
@Outcomes(Array(
  new Outcome(id = Array("z, List(x, y)", "z, List(y, x)"), expect = ACCEPTABLE, desc = "Pop is the first"),
  new Outcome(id = Array("x, List(y, z)", "y, List(x, z)"), expect = ACCEPTABLE, desc = "Pop one of the pushed values"),
))
abstract class TreiberStackTest(impl: KCAS) {

  protected implicit final val kcasImpl: KCAS =
    impl

  private[this] val stack =
    new TreiberStack[String](List("z"))

  private[this] val push =
    stack.push

  private[this] val tryPop =
    stack.tryPop

  @Actor
  def push1(): Unit = {
    push.unsafePerform("x")
  }

  @Actor
  def push2(): Unit = {
    push.unsafePerform("y")
  }

  @Actor
  def pop(r: LL_Result): Unit = {
    r.r1 = tryPop.unsafeRun.get
  }

  @Arbiter
  def arbiter(r: LL_Result): Unit = {
    r.r2 = stack.unsafeToList
  }
}
