package com.example.rea

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.infra.results.LL_Result

import kcas._

@JCStressTest
@State
@Description("Treiber stack pop/push should be atomic (MCAS)")
class TreiberStackTestMCAS extends TreiberStackTest(KCAS.MCAS) {

  @Actor
  override def push1(): Unit =
    super.push1()

  @Actor
  override def push2(): Unit =
    super.push2()

  @Actor
  override def pop(r: LL_Result): Unit =
    super.pop(r)

  @Arbiter
  override def arbiter(r: LL_Result): Unit =
    super.arbiter(r)
}
