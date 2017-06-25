package io.sigs.choam
package bench

import org.openjdk.jmh.annotations.{ Benchmark, Warmup, Fork, Measurement, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class StackTransferBench {

  import StackTransferBench._

  @Benchmark
  @Measurement(iterations = 20)
  def treiberStack(s: TreiberSt, bh: Blackhole, ct: KCASThreadState): Unit = {
    import ct.kcasImpl
    bh.consume(s.treiberStack1.push.unsafePerform(ct.nextString()))
    bh.consume(s.transfer.unsafeRun)
    if (s.treiberStack2.tryPop.unsafeRun eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def lockedStack(s: LockedSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.lockedStack1.push(ct.nextString()))

    s.lockedStack1.lock.lock()
    s.lockedStack2.lock.lock()
    try {
      val item = s.lockedStack1.unlockedTryPop().get
      bh.consume(s.lockedStack2.unlockedPush(item))
    } finally {
      s.lockedStack1.lock.unlock()
      s.lockedStack2.lock.unlock()
    }

    if (s.lockedStack2.tryPop() eq None) throw Errors.EmptyStack

    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def stmStack(s: StmSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    import scala.concurrent.stm._
    bh.consume(s.stmStack1.push(ct.nextString()))
    bh.consume(atomic { implicit txn =>
      val item = s.stmStack1.tryPop().get
      s.stmStack2.push(item)
    })
    if (s.stmStack2.tryPop() eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.tokens)
  }
}

object StackTransferBench {

  @State(Scope.Benchmark)
  class TreiberSt {
    val treiberStack1 =
      new TreiberStack[String](Prefill.prefill())
    val treiberStack2 =
      new TreiberStack[String](Prefill.prefill())
    val transfer =
      treiberStack1.tryPop.map(_.get) >>> treiberStack2.push
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedStack1 =
      new LockedStack[String](Prefill.prefill())
    val lockedStack2 =
      new LockedStack[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmStack1 =
      new StmStack[String](Prefill.prefill())
    val stmStack2 =
      new StmStack[String](Prefill.prefill())
  }
}
