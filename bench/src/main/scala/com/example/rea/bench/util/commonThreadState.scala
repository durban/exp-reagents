package com.example.rea
package bench
package util

import org.openjdk.jmh.annotations.{ State, Param, Setup, Scope }

import kcas._

@State(Scope.Thread)
class CommonThreadState {

  private[this] val rnd =
    XorShift()

  /** Approximately 0.3µs (i5-4300M) */
  private[this] val baseTokens: Long =
    128

  /**
   * Amount to left-shift `baseTokens`:
   * - 0 for high contention (≅ 0.3µs)
   * - 4 for low contention (≅ 4.8µs)
   */
  @Param(Array("0", "4"))
  private[this] var contention: Int = _

  def tokens: Long =
    baseTokens << contention

  def halfTokens: Long =
    baseTokens << (contention - 1)

  def nextInt(): Int =
    rnd.nextInt()

  def nextLong(): Long =
    rnd.nextLong()

  def nextString(): String =
    rnd.nextLong().abs.toString
}

@State(Scope.Thread)
class KCASThreadState extends CommonThreadState {

  @Param(Array(KCAS.fqns.CASN, KCAS.fqns.NaiveKCAS, KCAS.fqns.MCAS))
  private[this] var kcasName: String = _

  private[rea] implicit var kcasImpl: KCAS = _

  @Setup
  def setupKCASImpl(): Unit = {
    this.kcasImpl = KCAS.unsafeLookup(kcasName)
  }
}
