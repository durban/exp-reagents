package com.example.rea

import org.openjdk.jmh.annotations.{ Benchmark, Scope, State }
import org.openjdk.jmh.infra.Blackhole

import kcas._

// bench/jmh:run -i 6 -wi 6 -f 1 -t max .*Bench.*
class StackBench {
  
  import StackBench._

  @Benchmark
  def treiberStack(s: TreiberSt, bh: Blackhole, ct: ThreadSt): Unit = {
    import s.kcas
    if (ct.shouldPush()) {
      bh.consume(s.treiberStack.push ! ct.item)
    } else {
      bh.consume(s.treiberStack.tryPop.run)
    }
  }

  @Benchmark
  def referenceStack(s: ReferenceSt, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      bh.consume(s.referenceStack.push(ct.item))
    } else {
      bh.consume(s.referenceStack.tryPop())
    }
  }

  @Benchmark
  def lockedStack(s: LockedSt, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      bh.consume(s.lockedStack.push(ct.item))
    } else {
      bh.consume(s.lockedStack.tryPop())
    }
  }

  @Benchmark
  def concurrentDeque(s: JdkSt, bh: Blackhole, ct: ThreadSt): Unit = {
    if (ct.shouldPush()) {
      bh.consume(s.concurrentDeque.push(ct.item))
    } else {
      bh.consume(s.concurrentDeque.pollFirst())
    }
  }
}

object StackBench {

  @State(Scope.Benchmark)
  class TreiberSt {
    implicit val kcas: KCAS =
      KCAS.CASN
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

  @State(Scope.Thread)
  class ThreadSt {

    val item =
      scala.util.Random.nextString(10)
    
    var count: Long =
      Long.MinValue

    def shouldPush(): Boolean = {
      count += 1L
      (count % 3L) == 0L
    }
  }
}
