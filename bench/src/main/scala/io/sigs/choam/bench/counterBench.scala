/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package io.sigs.choam
package bench

import org.openjdk.jmh.annotations.{ Benchmark, Measurement, Warmup, Fork, State, Setup, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class CounterBench {

  import CounterBench._

  @Benchmark
  def reference(s: ReferenceSt, t: CommonThreadState, bh: Blackhole): Unit = {
    bh.consume(s.referenceCtr.add(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def locked(s: LockedSt, t: CommonThreadState, bh: Blackhole): Unit = {
    bh.consume(s.lockedCtr.add(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def react(s: ReactSt, t: KCASThreadState, bh: Blackhole): Unit = {
    import t.kcasImpl
    bh.consume(s.reactCtr.add.unsafePerform(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }
}

object CounterBench {

  @State(Scope.Benchmark)
  class ReferenceSt {
    val referenceCtr = {
      val ctr = new ReferenceCounter
      val init = java.util.concurrent.ThreadLocalRandom.current().nextLong()
      ctr.add(init)
      ctr
    }
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedCtr = {
      val ctr = new LockedCounter
      val init = java.util.concurrent.ThreadLocalRandom.current().nextLong()
      ctr.add(init)
      ctr
    }
  }

  @State(Scope.Benchmark)
  class ReactSt {
    val reactCtr = {
      val ctr = new Counter
      val init = java.util.concurrent.ThreadLocalRandom.current().nextLong()
      ctr.add.unsafePerform(init)(kcas.KCAS.NaiveKCAS)
      ctr
    }
  }
}

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class CounterBenchN {

  import CounterBenchN._

  @Benchmark
  @Measurement(iterations = 20)
  def lockedN(s: LockedStN, t: CommonThreadState, bh: Blackhole): Unit = {
    bh.consume(s.lockedCtrN.add(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def reactN(s: ReactStN, t: KCASThreadState, bh: Blackhole): Unit = {
    import t.kcasImpl
    bh.consume(s.r.unsafePerform(t.nextLong()))
    Blackhole.consumeCPU(t.tokens)
  }
}

object CounterBenchN {

  @State(Scope.Benchmark)
  class LockedStN {

    private[this] final val n = 8

    @volatile
    var lockedCtrN: LockedCounterN = _

    @Setup
    def setup(): Unit = {
      val ctr = new LockedCounterN(n)
      val init = java.util.concurrent.ThreadLocalRandom.current().nextLong()
      ctr.add(init)
      lockedCtrN = ctr
    }
  }

  @State(Scope.Benchmark)
  class ReactStN {

    private[this] final val n = 8

    private[this] var ctrs: Array[Counter] = _

    @volatile
    var r: React[Long, Unit] = _

    @Setup
    def setup(): Unit = {
      val init = java.util.concurrent.ThreadLocalRandom.current().nextLong()
      ctrs = Array.fill(n) {
        val c = new Counter
        c.add.unsafePerform(init)(kcas.KCAS.NaiveKCAS)
        c
      }
      r = ctrs.map(_.add.rmap(_ => ())).reduceLeft { (a, b) => (a * b).rmap(_ => ()) }
    }
  }
}
