package com.example.rea

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }
import org.openjdk.jmh.infra.Blackhole

import kcas._

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
  def react(s: ReactSt, t: ThreadSt, bh: Blackhole): Unit = {
    import s.kcas
    bh.consume(s.reactCtr.add.unsafePerform(t.nextItem()))
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
    implicit val kcas: KCAS =
      KCAS.CASN
    val reactCtr =
      new Counter
  }

  @State(Scope.Thread)
  class ThreadSt {

    private[this] val rnd =
      java.util.concurrent.ThreadLocalRandom.current()

    def nextItem(): Long =
      rnd.nextLong()
  }
}
