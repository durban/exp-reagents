package io.sigs.choam
package bench

import org.openjdk.jmh.annotations.{ Benchmark, Warmup, Fork, Measurement, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class StackBench {

  import StackBench._

  @Benchmark
  def treiberStack(s: TreiberSt, bh: Blackhole, ct: KCASThreadState): Unit = {
    import ct.kcasImpl
    bh.consume(s.treiberStack.push.unsafePerform(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.treiberStack.tryPop.unsafeRun eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }

  @Benchmark
  @Measurement(iterations = 20)
  def referenceStack(s: ReferenceSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.referenceStack.push(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.referenceStack.tryPop() eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }

  @Benchmark
  def lockedStack(s: LockedSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.lockedStack.push(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.lockedStack.tryPop() eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }

  @Benchmark
  def stmStack(s: StmSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.stmStack.push(ct.nextString()))
    Blackhole.consumeCPU(ct.halfTokens)
    if (s.stmStack.tryPop() eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.halfTokens)
  }
}

object StackBench {

  @State(Scope.Benchmark)
  class TreiberSt {
    val treiberStack = new TreiberStack[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class ReferenceSt {
    val referenceStack = new ReferenceTreiberStack[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedStack = new LockedStack[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmStack = new StmStack[String](Prefill.prefill())
  }
}
