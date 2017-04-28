package com.example.rea

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }
import org.openjdk.jmh.infra.Blackhole

import kcas._

/**
 * bench/jmh:run -i 6 -wi 6 -f 1 -t max .*Bench.*
 */
class Bench {

  import Bench._

  @Benchmark
  def treiberStack(s: SharedState, bh: Blackhole, ct: ThreadSt): Unit = {
    import s.kcas
    if (ct.shouldPush()) {
      s.treiberStack.push ! s.item
    } else {
      bh.consume(s.treiberStack.tryPop.run)
    }
  }

  @Benchmark
  def referenceStack(s: SharedState, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      s.referenceStack.push(s.item)
    } else {
      bh.consume(s.referenceStack.tryPop())
    }
  }

  @Benchmark
  def lockedStack(s: SharedState, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      s.lockedStack.push(s.item)
    } else {
      bh.consume(s.lockedStack.tryPop())
    }
  }

  @Benchmark
  def concurrentDeque(s: SharedState, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      s.concurrentDeque.push(s.item)
    } else {
      bh.consume(s.concurrentDeque.pollFirst())
    }
  }
}

object Bench {

  final val TreiberStack = "treiberStack"
  final val ReferenceStack = "referenceStack"
  final val LockedStack = "lockedStack"

  final val Threshold = 0.3

  @State(Scope.Benchmark)
  class SharedState {
    implicit val kcas: KCAS = KCAS.CASN
    val treiberStack = new TreiberStack[String]
    val referenceStack = new ReferenceTreiberStack[String]
    val lockedStack = new LockedStack[String]
    val concurrentDeque = new java.util.concurrent.ConcurrentLinkedDeque[String]
    val item = scala.util.Random.nextString(10)
  }

  @State(Scope.Thread)
  class ThreadSt {

    var count: Long = Long.MinValue

    def shouldPush(): Boolean = {
      count += 1L
      (count % 3L) == 0L
    }
  }
}
