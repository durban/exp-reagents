package com.example.rea
package bench

import scala.util.control.NoStackTrace

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

class StackBench {

  import StackBench._

  @Benchmark
  def treiberStack(s: TreiberSt, bh: Blackhole, ct: KCASThreadState): Unit = {
    import ct.kcasImpl
    bh.consume(s.treiberStack.push.unsafePerform(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.treiberStack.tryPop.unsafeRun eq None) throw EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }

  @Benchmark
  def referenceStack(s: ReferenceSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.referenceStack.push(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.referenceStack.tryPop() eq None) throw EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }

  @Benchmark
  def lockedStack(s: LockedSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.lockedStack.push(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.lockedStack.tryPop() eq None) throw EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }

  @Benchmark
  def concurrentDeque(s: JdkSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.concurrentDeque.push(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.concurrentDeque.pollFirst() eq null) throw EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }

  @Benchmark
  def stmStack(s: StmSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.stmStack.push(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.stmStack.tryPop() eq None) throw EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }
}

object StackBench {

  object EmptyStack extends AssertionError with NoStackTrace

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
}
