package io.sigs.choam
package kcas
package bench

import java.util.concurrent.ThreadLocalRandom

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import io.sigs.choam.bench.util._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@BenchmarkMode(Array(Mode.AverageTime))
class FailedCAS1Bench {

  import KCASBenchHelpers._

  @Benchmark
  def failedCAS1(r: RefState, t: KCASThreadState): Unit = {
    val succ = t.kcasImpl.start().withCAS(r.ref, incorrectOv, t.nextString()).tryPerform()
    if (succ) throw new AssertionError("CAS should've failed")
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def failedCAS1Reference(r: RefState, t: CommonThreadState): Unit = {
    val succ = r.ref.unsafeTryPerformCas(incorrectOv, t.nextString())
    if (succ) throw new AssertionError("CAS should've failed")
    Blackhole.consumeCPU(t.tokens)
  }
}

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@BenchmarkMode(Array(Mode.AverageTime))
class CAS1LoopBench {

  import KCASBenchHelpers._

  @Benchmark
  def successfulCAS1Loop(r: RefState, t: KCASThreadState): Unit = {
    val ref = r.ref
    val kcasImpl = t.kcasImpl
    @tailrec
    def go(): Unit = {
      val ov = kcasImpl.read(ref)
      val nv = (ov.toLong + t.nextLong()).toString
      val succ = kcasImpl.start().withCAS(ref, ov, nv).tryPerform()
      if (succ) ()
      else go()
    }
    go()
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def successfulCAS1LoopReference(r: RefState, t: CommonThreadState): Unit = {
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
}

object KCASBenchHelpers {

  final val incorrectOv = "no such number"

  @State(Scope.Benchmark)
  class RefState {
    val ref = kcas.Ref.mk(ThreadLocalRandom.current().nextLong().toString)
  }
}