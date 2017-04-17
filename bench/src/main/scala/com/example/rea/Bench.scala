package com.example.rea

import java.util.concurrent.ThreadLocalRandom

import org.openjdk.jmh.annotations.{ Benchmark, State, Scope }

import kcas._

/**
 * bench/jmh:run -i 6 -wi 6 -f 1 -t max .*Bench.*
 */
class Bench {

  import Bench._

  @Benchmark
  def treiberStack(s: SharedState, c: Ctr): Unit = {
    import s.kcas
    if ((c.cnt < MaxSize) && c.rnd.nextBoolean()) {
      c.cnt += 1
      s.treiberStack.push ! s.item
    } else {
      if (s.treiberStack.tryPop.run.isDefined) {
        c.cnt -= 1
      }
    }
  }

  @Benchmark
  def referenceStack(s: SharedState, c: Ctr): Unit = {
    if ((c.cnt < MaxSize) && c.rnd.nextBoolean()) {
      c.cnt += 1
      s.referenceStack.push(s.item)
    } else {
      if (s.referenceStack.tryPop().isDefined) {
        c.cnt -= 1
      }
    }
  }

  @Benchmark
  def lockedStack(s: SharedState, c: Ctr): Unit = {
    if ((c.cnt < MaxSize) && c.rnd.nextBoolean()) {
      c.cnt += 1
      s.lockedStack.push(s.item)
    } else {
      if (s.lockedStack.tryPop().isDefined) {
        c.cnt -= 1
      }
    }
  }
  
  @Benchmark
  def concurrentDeque(s: SharedState, c: Ctr): Unit = {
    if ((c.cnt < MaxSize) && c.rnd.nextBoolean()) {
      c.cnt += 1
      s.concurrentDeque.push(s.item)
    } else {
      if (s.concurrentDeque.pollFirst() ne null) {
        c.cnt -= 1
      }
    }
  }
}

object Bench {

  final val TreiberStack = "treiberStack"
  final val ReferenceStack = "referenceStack"
  final val LockedStack = "lockedStack"
  
  final val MaxSize = 10000
  
  @State(Scope.Benchmark)
  class SharedState {
    implicit val kcas: KCAS = KCAS.CASN
    val treiberStack = new TreiberStack[String]
    val referenceStack = new ReferenceTreiberStack[String]
    val lockedStack = new LockedStack[String]
    val concurrentDeque = new java.util.concurrent.ConcurrentLinkedDeque[String]
    val item = ThreadLocalRandom.current().nextInt().toString
  }
  
  @State(Scope.Thread)
  class Ctr {
    var cnt = 0
    val rnd = java.util.concurrent.ThreadLocalRandom.current()
  }
}
