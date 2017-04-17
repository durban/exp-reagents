package com.example.rea

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import kcas._

/**
 * bench/jmh:run -i 6 -wi 6 -f 1 -t max .*Bench.*
 */
class Bench {

  import Bench._

  @Benchmark
  def treiberStack(s: SharedState, bh: Blackhole): Unit = {
    import s.kcas
    val len = s.stack.length.run
    if (len < 1000) {
      s.stack.push ! (len + 1)
    } else {
      val i = s.stack.tryPop.run
      bh.consume(i)
    }
  }

  @Benchmark
  def referenceTreiberStack(s: SharedState, bh: Blackhole): Unit = {
    val len = s.referenceStack.length
    if (len < 1000) {
      s.referenceStack.push(len + 1)
    } else {
      val i = s.referenceStack.tryPop()
      bh.consume(i)
    }
  }
}

object Bench {

  @State(Scope.Benchmark)
  class SharedState {
    implicit val kcas: KCAS = KCAS.CASN
    val stack = new TreiberStack[Int]
    val referenceStack = new ReferenceTreiberStack[Int]
  }
}
