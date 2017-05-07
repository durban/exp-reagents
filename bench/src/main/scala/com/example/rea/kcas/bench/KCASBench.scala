package com.example.rea
package kcas
package bench

import java.util.concurrent.ThreadLocalRandom

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }

import com.example.rea.bench.util._

class KCASBench {

  import KCASBench._

  @Benchmark
  def failedCAS1(r: RefState, t: KCASThreadState): Unit = {
    val succ = t.kcasImpl.tryPerform(KCASD(CASD(r.ref, r.incorrect, t.nextString()) :: Nil))
    if (succ) throw new AssertionError("CAS should've failed")
  }

  @Benchmark
  def failedCAS1Baseline(r: RefState, t: CommonThreadState): Unit = {
    val succ = r.ref.unsafeTryPerformCas(r.incorrect, t.nextString())
    if (succ) throw new AssertionError("CAS should've failed")
  }

  @Benchmark
  def successfulCAS1Loop(r: RefState, t: KCASThreadState): Unit = {
    val ref = r.ref
    val kcasImpl = t.kcasImpl
    @tailrec
    def go(): Unit = {
      val ov = kcasImpl.tryReadOne(ref)
      val nv = (ov.toLong + t.nextLong()).toString
      val succ = kcasImpl.tryPerform(KCASD(CASD(ref, ov, nv) :: Nil))
      if (succ) ()
      else go()
    }
    go()
  }

  @Benchmark
  def successfulCAS1LoopBaseline(r: RefState, t: CommonThreadState): Unit = {
    val ref = r.ref
    @tailrec
    def go(): Unit = {
      val ov = ref.unsafeTryRead()
      val nv = (ov.toLong + t.nextLong()).toString
      val succ = ref.unsafeTryPerformCas(ov, nv)
      if (succ) ()
      else go()
    }
    go()
  }
}

object KCASBench {

  @State(Scope.Benchmark)
  class RefState {
    private[this] val rnd = ThreadLocalRandom.current()
    val incorrect = "no such number"
    val ref = kcas.Ref.mk(rnd.nextLong().toString)
  }
}
