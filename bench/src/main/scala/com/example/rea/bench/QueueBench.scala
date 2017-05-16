package com.example.rea
package bench

import scala.util.control.NoStackTrace

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import com.example.rea.kcas._
import util._

class QueueBench {

  import QueueBench._

  @Benchmark
  def michaelScottQueue(s: MsSt, bh: Blackhole, t: KCASThreadState): Unit = {
    import t.kcasImpl
    bh.consume(s.michaelScottQueue.enqueue.unsafePerform(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.michaelScottQueue.tryDeque.unsafeRun eq None) throw EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }

  @Benchmark
  def lockedQueueProducer(s: LockedSt, bh: Blackhole, t: CommonThreadState): Unit = {
    bh.consume(s.lockedQueue.enqueue(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.lockedQueue.tryDequeue() eq None) throw EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }

  @Benchmark
  def concurrentQueueProducer(s: JdkSt, bh: Blackhole, t: CommonThreadState): Unit = {
    bh.consume(s.concurrentQueue.offer(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.concurrentQueue.poll() eq null) throw EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }

  @Benchmark
  def stmQueueProducer(s: StmSt, bh: Blackhole, t: CommonThreadState): Unit = {
    bh.consume(s.stmQueue.enqueue(t.nextString()))
    Blackhole.consumeCPU(t.halfTokens)
    if (s.stmQueue.tryDequeue() eq None) throw EmptyQueue
    Blackhole.consumeCPU(t.halfTokens)
  }
}

object QueueBench {

  object EmptyQueue extends AssertionError with NoStackTrace

  private[this] final val prefill = (1 to 10000)

  private[this] def prefillItem(): String =
    scala.util.Random.nextString(16)

  @State(Scope.Benchmark)
  class MsSt {
    val michaelScottQueue = {
      implicit val prefillKcas: KCAS = KCAS.NaiveKCAS
      val q = new MichaelScottQueue[String]
      for (_ <- prefill) { q.enqueue.unsafePerform(prefillItem()) }
      q
    }
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedQueue = {
      val q = new LockedQueue[String]
      for (_ <- prefill) { q.enqueue(prefillItem()) }
      q
    }
  }

  @State(Scope.Benchmark)
  class JdkSt {
    val concurrentQueue = {
      val q = new java.util.concurrent.ConcurrentLinkedQueue[String]
      for (_ <- prefill) { q.offer(prefillItem()) }
      q
    }
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmQueue = {
      val q = new StmQueue[String]
      for (_ <- prefill) { q.enqueue(prefillItem()) }
      q
    }
  }
}
