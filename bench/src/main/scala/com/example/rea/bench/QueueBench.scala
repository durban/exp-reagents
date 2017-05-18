package com.example.rea
package bench

import org.openjdk.jmh.annotations.{ Benchmark, Warmup, Measurement, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

@Warmup(iterations = 10)
@Measurement(iterations = 10)
class QueueBench {

  import QueueBench._

  @Benchmark
  def michaelScottQueue(s: MsSt, bh: Blackhole, t: KCASThreadState): Unit = {
    import t.kcasImpl
    bh.consume(s.michaelScottQueue.enqueue.unsafePerform(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.michaelScottQueue.tryDeque.unsafeRun eq None) throw Errors.EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }

  @Benchmark
  def lockedQueueProducer(s: LockedSt, bh: Blackhole, t: CommonThreadState): Unit = {
    bh.consume(s.lockedQueue.enqueue(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.lockedQueue.tryDequeue() eq None) throw Errors.EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }

  @Benchmark
  def concurrentQueueProducer(s: JdkSt, bh: Blackhole, t: CommonThreadState): Unit = {
    bh.consume(s.concurrentQueue.offer(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.concurrentQueue.poll() eq null) throw Errors.EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }

  @Benchmark
  def stmQueueProducer(s: StmSt, bh: Blackhole, t: CommonThreadState): Unit = {
    bh.consume(s.stmQueue.enqueue(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.stmQueue.tryDequeue() eq None) throw Errors.EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }
}

object QueueBench {

  @State(Scope.Benchmark)
  class MsSt {
    val michaelScottQueue = new MichaelScottQueue[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedQueue = new LockedQueue[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class JdkSt {
    val concurrentQueue = new java.util.concurrent.ConcurrentLinkedQueue[String](Prefill.forJava())
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmQueue = new StmQueue[String](Prefill.prefill())
  }
}
