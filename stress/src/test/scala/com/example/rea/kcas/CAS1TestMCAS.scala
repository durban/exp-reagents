package com.example.rea
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.infra.results.ZZL_Result

@JCStressTest
@State
@Description("CAS1 should be atomic (MCAS)")
class CAS1TestMCAS extends CAS1Test(MCAS) {

  @Actor
  override def writer1(r: ZZL_Result): Unit =
    super.writer1(r)

  @Actor
  override def writer2(r: ZZL_Result): Unit =
    super.writer2(r)

  @Actor
  override def reader(r: ZZL_Result): Unit =
    super.reader(r)

  @Arbiter
  override def arbiter(r: ZZL_Result): Unit =
    super.arbiter(r)
}
