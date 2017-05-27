package com.example.rea
package bench
package util

import org.openjdk.jmh.annotations.{ State, Param, Setup, Scope }

import kcas._

@State(Scope.Thread)
class CommonThreadState {

  private[this] val rnd =
    XorShift()

  val tokens: Long =
    64

  val halfTokens: Long =
    tokens >>> 1

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
