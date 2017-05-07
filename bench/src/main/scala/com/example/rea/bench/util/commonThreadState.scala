package com.example.rea
package bench
package util

import org.openjdk.jmh.annotations.{ State, Param, Setup, Scope }

import kcas._

@State(Scope.Thread)
class CommonThreadState {

  private[this] val rnd =
    java.util.concurrent.ThreadLocalRandom.current()

  def nextLong(): Long =
    rnd.nextLong()

  def nextString(): String =
    nextLong().toString
}

@State(Scope.Thread)
class KCASThreadState extends CommonThreadState {

  @Param(Array(KCAS.fqns.CASN, KCAS.fqns.NaiveKCAS))
  private[this] var kcasName: String = _

  private[rea] implicit var kcasImpl: KCAS = _

  @Setup
  def setupKCASImpl(): Unit = {
    this.kcasImpl = KCAS.unsafeLookup(kcasName)
  }
}
