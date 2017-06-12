package com.example.rea
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.infra.results.ZL_Result

@JCStressTest
@State
@Description("k-CAS should be atomic to readers (MCAS)")
class KCASReadTestMCAS extends KCASReadTest(MCAS) {

  @Actor
  override def writer(r: ZL_Result): Unit =
    super.writer(r)

  @Actor
  override def reader(r: ZL_Result): Unit =
    super.reader(r)
}
