package com.example.rea
package bench
package util

import org.openjdk.jmh.annotations.{ State, Param, Setup, Scope }

import kcas._

@State(Scope.Thread)
class RandomState {

  private[this] val rnd =
    XorShift()

  def nextInt(): Int =
    rnd.nextInt()

  def nextLong(): Long =
    rnd.nextLong()

  def nextString(): String =
    rnd.nextLong().abs.toString
}

object CommonThreadState {

  /** Approximately 0.3µs (i5-4300M) */
  final val BaseTokens = 128L

  /** BaseTokens << 0 */
  final val LowContention = 0

  final val LowContentionS = "0"

  /** BaseTokens << 4 (≅ 4.8µs) */
  final val HighContention = 4

  final val HighContentionS = "4"
}

@State(Scope.Thread)
class CommonThreadState extends RandomState {

  import CommonThreadState._

  @Param(Array(LowContentionS, HighContentionS))
  private[this] var contention: Int = _

  def tokens: Long =
    BaseTokens << contention

  def halfTokens: Long =
    BaseTokens << (contention - 1)
}

@State(Scope.Thread)
trait KCASImplState {

  @Param(Array(KCAS.fqns.CASN, KCAS.fqns.NaiveKCAS, KCAS.fqns.MCAS))
  private[rea] var kcasName: String = _

  private[rea] implicit var kcasImpl: KCAS = _

  @Setup
  def setupKCASImpl(): Unit = {
    this.kcasImpl = KCAS.unsafeLookup(kcasName)
  }
}

@State(Scope.Thread)
class KCASThreadState extends CommonThreadState with KCASImplState
