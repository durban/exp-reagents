package com.example.rea
package bench

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

class StackBench {

  import StackBench._

  @Benchmark
  def treiberStack(s: TreiberSt, bh: Blackhole, ct: KCASThreadSt): Unit = {
    import ct.kcasImpl
    if (ct.shouldPush()) {
      bh.consume(s.treiberStack.push.unsafePerform(ct.nextString()))
    } else {
      bh.consume(s.treiberStack.tryPop.unsafeRun)
    }
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def referenceStack(s: ReferenceSt, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      bh.consume(s.referenceStack.push(ct.nextString()))
    } else {
      bh.consume(s.referenceStack.tryPop())
    }
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def lockedStack(s: LockedSt, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      bh.consume(s.lockedStack.push(ct.nextString()))
    } else {
      bh.consume(s.lockedStack.tryPop())
    }
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def concurrentDeque(s: JdkSt, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      bh.consume(s.concurrentDeque.push(ct.nextString()))
    } else {
      bh.consume(s.concurrentDeque.pollFirst())
    }
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def stmStack(s: StmSt, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      bh.consume(s.stmStack.push(ct.nextString()))
    } else {
      bh.consume(s.stmStack.tryPop())
    }
    Blackhole.consumeCPU(ct.tokens)
  }
}

object StackBench {

  @State(Scope.Benchmark)
  class TreiberSt {
    val treiberStack =
      new TreiberStack[String]
  }

  @State(Scope.Benchmark)
  class ReferenceSt {
    val referenceStack =
      new ReferenceTreiberStack[String]
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedStack =
      new LockedStack[String]
  }

  @State(Scope.Benchmark)
  class JdkSt {
    val concurrentDeque =
      new java.util.concurrent.ConcurrentLinkedDeque[String]
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmStack =
      new StmStack[String]
  }

  @State(Scope.Thread)
  class ThreadSt extends CommonThreadState {

    var count: Long =
      Long.MinValue

    def shouldPush(): Boolean = {
      count += 1L
      (count % 3L) == 0L
    }
  }

  @State(Scope.Thread)
  class KCASThreadSt extends KCASThreadState {

    var count: Long =
      Long.MinValue

    def shouldPush(): Boolean = {
      count += 1L
      (count % 3L) == 0L
    }
  }
}
