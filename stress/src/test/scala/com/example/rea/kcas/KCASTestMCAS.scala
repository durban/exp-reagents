package com.example.rea
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.infra.results.ZZL_Result

@JCStressTest
@State
@Description("k-CAS should be atomic (MCAS)")
class KCASTestMCAS extends KCASTest(MCAS) {

  @Actor
  override def writer1(r: ZZL_Result): Unit =
    super.writer1(r)

  @Actor
  override def writer2(r: ZZL_Result): Unit =
    super.writer2(r)

  @Arbiter
  override def arbiter(r: ZZL_Result): Unit =
    super.arbiter(r)
}
