package com.example.rea
package bench

import org.openjdk.jmh.annotations.{ Benchmark, State, Param, Setup, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

class CounterBench {

  import CounterBench._

  @Benchmark
  def baseline(t: CommonThreadState): Unit = {
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def reference(s: ReferenceSt, t: CommonThreadState, bh: Blackhole): Unit = {
    bh.consume(s.referenceCtr.add(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def locked(s: LockedSt, t: CommonThreadState, bh: Blackhole): Unit = {
    bh.consume(s.lockedCtr.add(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def react(s: ReactSt, t: KCASThreadState, bh: Blackhole): Unit = {
    import t.kcasImpl
    bh.consume(s.reactCtr.add.unsafePerform(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }
}

object CounterBench {

  @State(Scope.Benchmark)
  class ReferenceSt {
    val referenceCtr =
      new ReferenceCounter
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedCtr =
      new LockedCounter
  }

  @State(Scope.Benchmark)
  class ReactSt {
    val reactCtr =
      new Counter
  }
}

class CounterBenchN {

  import CounterBenchN._

  @Benchmark
  def lockedN(s: LockedStN, t: CommonThreadState, bh: Blackhole): Unit = {
    bh.consume(s.lockedCtrN.add(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def reactN(s: ReactStN, t: KCASThreadState, bh: Blackhole): Unit = {
    import t.kcasImpl
    bh.consume(s.r.unsafePerform(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }
}

object CounterBenchN {

  @State(Scope.Benchmark)
  class LockedStN {

    @Param(Array("2", "8"))
    private[this] var n: Int = _

    @volatile
    var lockedCtrN: LockedCounterN = _

    @Setup
    def setup(): Unit = {
      lockedCtrN = new LockedCounterN(n)
    }
  }

  @State(Scope.Benchmark)
  class ReactStN {

    @Param(Array("2", "8"))
    private[this] var n: Int = _

    private[this] var ctrs: Array[Counter] = _

    @volatile
    var r: React[Long, Unit] = _

    @Setup
    def setup(): Unit = {
      ctrs = Array.fill(n)(new Counter)
      r = ctrs.map(_.add.rmap(_ => ())).reduceLeft { (a, b) => (a * b).rmap(_ => ()) }
    }
  }
}
