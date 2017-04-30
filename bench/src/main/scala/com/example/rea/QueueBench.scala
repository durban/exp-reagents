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
    bh.consume(s.michaelScottQueue.enqueue ! t.item)
    Blackhole.consumeCPU(producerWaitTime)
  }

  @Benchmark
  @Group("MS")
  def michaelScottQueueConsumer(s: MsSt, bh: Blackhole): Unit = {
    import s.kcas
    bh.consume(s.michaelScottQueue.tryDeque.run)
    Blackhole.consumeCPU(consumerWaitTime)
  }

  @Benchmark
  @Group("LCK")
  def lockedQueueProducer(s: LockedSt, bh: Blackhole, t: ThreadSt): Unit = {
    bh.consume(s.lockedQueue.enqueue(t.item))
    Blackhole.consumeCPU(producerWaitTime)
  }

  @Benchmark
  @Group("LCK")
  def lockedQueueConsumer(s: LockedSt, bh: Blackhole): Unit = {
    bh.consume(s.lockedQueue.tryDequeue())
    Blackhole.consumeCPU(consumerWaitTime)
  }
}

object QueueBench {

  final val producerWaitTime = 19L
  final val consumerWaitTime = 10L

  @State(Scope.Benchmark)
  class MsSt {
    implicit val kcas: KCAS =
      KCAS.CASN
    val michaelScottQueue =
      new MichaelScottQueue[String]
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedQueue =
      new LockedQueue[String]
  }

  @State(Scope.Thread)
  class ThreadSt {
    val item =
      scala.util.Random.nextString(10)
  }
}
