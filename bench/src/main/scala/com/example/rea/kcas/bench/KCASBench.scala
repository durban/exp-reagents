package com.example.rea
package kcas
package bench

import java.util.concurrent.ThreadLocalRandom

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import com.example.rea.bench.util._

class KCASBench {

  import KCASBench._

  @tailrec
  private[this] def read(ref: Ref[String], kcas: KCAS): String = {
    val r = kcas.tryReadOne(ref)
    if (r eq null) read(ref, kcas)
    else r
  }

  @Benchmark
  def failedCAS1(r: RefState, t: KCASThreadState): Unit = {
    val succ = t.kcasImpl.tryPerform(KCASD(CASD(r.ref, incorrectOv, t.nextString()) :: Nil))
    if (succ) throw new AssertionError("CAS should've failed")
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def failedCAS1Baseline(r: RefState, t: CommonThreadState): Unit = {
    val succ = r.ref.unsafeTryPerformCas(incorrectOv, t.nextString())
    if (succ) throw new AssertionError("CAS should've failed")
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def successfulCAS1Loop(r: RefState, t: KCASThreadState): Unit = {
    val ref = r.ref
    val kcasImpl = t.kcasImpl
    @tailrec
    def go(): Unit = {
      val ov = read(ref, kcasImpl)
      val nv = (ov.toLong + t.nextLong()).toString
      val succ = kcasImpl.tryPerform(KCASD(CASD(ref, ov, nv) :: Nil))
      if (succ) ()
      else go()
    }
    go()
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def successfulKCASLoop(r: KRefState, t: KCASThreadState): Unit = {
    val refs = r.refs
    val kcasImpl = t.kcasImpl
    @tailrec
    def go(): Unit = {
      val ds = refs.map { ref =>
        val ov = read(ref, kcasImpl)
        val nv = (ov.toLong + t.nextLong()).toString
        CASD(ref, ov, nv)
      }
      val succ = kcasImpl.tryPerform(KCASD(ds))
      if (succ) ()
      else go()
    }
    go()
    Blackhole.consumeCPU(t.tokens)
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
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def successfulKCASLoopBaseline(r: KRefState, t: CommonThreadState): Unit = {
    val refs = r.refs
    @tailrec
    def goOne(ref: Ref[String]): Unit = {
      val ov = ref.unsafeTryRead()
      val nv = (ov.toLong + t.nextLong()).toString
      val succ = ref.unsafeTryPerformCas(ov, nv)
      if (succ) ()
      else goOne(ref)
    }
    for (ref <- refs) {
      goOne(ref)
    }
    Blackhole.consumeCPU(t.tokens)
  }
}

object KCASBench {

  final val incorrectOv = "no such number"
  final val K = 8

  @State(Scope.Benchmark)
  class RefState {
    val ref = kcas.Ref.mk(ThreadLocalRandom.current().nextLong().toString)
  }

  @State(Scope.Benchmark)
  class KRefState {
    val refs = List.fill(K) {
      kcas.Ref.mk(ThreadLocalRandom.current().nextLong().toString)
    }
  }
}
