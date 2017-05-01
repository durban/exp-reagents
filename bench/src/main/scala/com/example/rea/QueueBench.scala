package com.example.rea

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State, Group }
import org.openjdk.jmh.infra.Blackhole

import kcas._

class QueueBench {

  import QueueBench._

  @Benchmark
  @Group("MS")
  def michaelScottQueueProducer(s: MsSt, bh: Blackhole, t: ThreadSt): Unit = {
    import s.kcas
    bh.consume(s.michaelScottQueue.enqueue.unsafePerform(t.nextItem()))
    Blackhole.consumeCPU(producerWaitTime)
  }

  @Benchmark
  @Group("MS")
  def michaelScottQueueConsumer(s: MsSt, bh: Blackhole): Unit = {
    import s.kcas
    bh.consume(s.michaelScottQueue.tryDeque.unsafeRun)
    Blackhole.consumeCPU(consumerWaitTime)
  }

  @Benchmark
  @Group("LCK")
  def lockedQueueProducer(s: LockedSt, bh: Blackhole, t: ThreadSt): Unit = {
    bh.consume(s.lockedQueue.enqueue(t.nextItem()))
    Blackhole.consumeCPU(producerWaitTime)
  }

  @Benchmark
  @Group("LCK")
  def lockedQueueConsumer(s: LockedSt, bh: Blackhole): Unit = {
    bh.consume(s.lockedQueue.tryDequeue())
    Blackhole.consumeCPU(consumerWaitTime)
  }

  @Benchmark
  @Group("JDK")
  def concurrentQueueProducer(s: JdkSt, bh: Blackhole, t: ThreadSt): Unit = {
    bh.consume(s.concurrentQueue.offer(t.nextItem()))
    Blackhole.consumeCPU(producerWaitTime)
  }

  @Benchmark
  @Group("JDK")
  def concurrentQueueConsumer(s: JdkSt, bh: Blackhole): Unit = {
    bh.consume(s.concurrentQueue.poll())
    Blackhole.consumeCPU(consumerWaitTime)
  }
}

object QueueBench {

  final val producerWaitTime = 19L
  final val consumerWaitTime = 10L

  final val prefill = (1 to 1000000)

  def prefillItem(): String =
    scala.util.Random.nextString(16)

  @State(Scope.Benchmark)
  class MsSt {
    implicit val kcas: KCAS =
      KCAS.CASN
    val michaelScottQueue = {
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

  @State(Scope.Thread)
  class ThreadSt {

    private[this] val rnd =
      java.util.concurrent.ThreadLocalRandom.current()

    def nextItem(): String =
      rnd.nextLong().toString
  }
}
