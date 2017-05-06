package com.example.rea
package bench

import org.openjdk.jmh.annotations.{ Benchmark, State, Param, Setup, Scope }
import org.openjdk.jmh.infra.Blackhole

import com.example.rea.kcas._
import util._

class CounterBench {

  import CounterBench._

  @Benchmark
  def reference(s: ReferenceSt, t: ThreadSt, bh: Blackhole): Unit = {
    bh.consume(s.referenceCtr.add(t.nextItem()))
  }

  @Benchmark
  def locked(s: LockedSt, t: ThreadSt, bh: Blackhole): Unit = {
    bh.consume(s.lockedCtr.add(t.nextItem()))
  }

  @Benchmark
  def lockedN(s: LockedStN, t: ThreadSt, bh: Blackhole): Unit = {
    bh.consume(s.lockedCtrN.add(t.nextItem()))
  }

  @Benchmark
  def react(s: ReactSt, t: ThreadSt, bh: Blackhole): Unit = {
    import s.kcas
    bh.consume(s.reactCtr.add.unsafePerform(t.nextItem()))
  }

  @Benchmark
  def reactN(s: ReactStN, t: ThreadSt, bh: Blackhole): Unit = {
    import s.kcas
    bh.consume(s.r.unsafePerform(t.nextItem()))
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
  class LockedStN {

    @Param(Array("2", "4", "8", "16"))
    private[this] var n: Int = _

    @volatile
    var lockedCtrN: LockedCounterN = _

    @Setup
    def setup(): Unit = {
      lockedCtrN = new LockedCounterN(n)
    }
  }

  @State(Scope.Benchmark)
  class ReactSt {
    implicit val kcas: KCAS =
      KCAS.CASN
    val reactCtr =
      new Counter
  }

  @State(Scope.Benchmark)
  class ReactStN {

    implicit val kcas: KCAS =
      KCAS.CASN

    @Param(Array("2", "4", "8", "16"))
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

  @State(Scope.Thread)
  class ThreadSt {

    private[this] val rnd =
      java.util.concurrent.ThreadLocalRandom.current()

    def nextItem(): Long =
      rnd.nextLong()
  }
}
