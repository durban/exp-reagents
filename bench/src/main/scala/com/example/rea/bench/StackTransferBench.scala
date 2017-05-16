package com.example.rea
package bench

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

class StackTransferBench {

  import StackTransferBench._

  @Benchmark
  def treiberStack(s: TreiberSt, bh: Blackhole, ct: KCASThreadState): Unit = {
    import ct.kcasImpl
    bh.consume(s.treiberStack1.push.unsafePerform(ct.nextString()))
    bh.consume(s.transfer.unsafeRun)
    bh.consume(s.treiberStack2.tryPop.unsafeRun)
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def lockedStack(s: LockedSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    s.lockedStack1.lock.lock()
    s.lockedStack2.lock.lock()
    try {
      bh.consume(s.lockedStack1.unlockedPush(ct.nextString()))
    } finally {
      s.lockedStack1.lock.unlock()
      s.lockedStack2.lock.unlock()
    }

    s.lockedStack1.lock.lock()
    s.lockedStack2.lock.lock()
    try {
      val item = s.lockedStack1.unlockedTryPop().getOrElse(Dummy)
      bh.consume(s.lockedStack2.unlockedPush(item))
    } finally {
      s.lockedStack1.lock.unlock()
      s.lockedStack2.lock.unlock()
    }

    s.lockedStack1.lock.lock()
    s.lockedStack2.lock.lock()
    try {
      bh.consume(s.lockedStack2.unlockedTryPop())
    } finally {
      s.lockedStack1.lock.unlock()
      s.lockedStack2.lock.unlock()
    }

    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def stmStack(s: StmSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    import scala.concurrent.stm._
    bh.consume(s.stmStack1.push(ct.nextString()))
    bh.consume(atomic { implicit txn =>
      val item = s.stmStack1.tryPop().getOrElse(Dummy)
      s.stmStack2.push(item)
    })
    bh.consume(s.stmStack2.tryPop())
    Blackhole.consumeCPU(ct.tokens)
  }
}

object StackTransferBench {

  final val Dummy = "0"

  @State(Scope.Benchmark)
  class TreiberSt {
    val treiberStack1 =
      new TreiberStack[String]
    val treiberStack2 =
      new TreiberStack[String]
    val transfer =
      treiberStack1.tryPop.map(_.getOrElse(Dummy)) >>> treiberStack2.push
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedStack1 =
      new LockedStack[String]
    val lockedStack2 =
      new LockedStack[String]
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmStack1 =
      new StmStack[String]
    val stmStack2 =
      new StmStack[String]
  }
}
