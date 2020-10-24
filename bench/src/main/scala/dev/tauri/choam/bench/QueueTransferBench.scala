/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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

package dev.tauri.choam
package bench

import org.openjdk.jmh.annotations.{ Benchmark, Warmup, Fork, Measurement, State, Scope }
import org.openjdk.jmh.infra.Blackhole

import util._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class QueueTransferBench {

  import QueueTransferBench._

  @Benchmark
  @Measurement(iterations = 20)
  def michaelScottQueue(s: MsSt, bh: Blackhole, ct: KCASThreadState): Unit = {
    import ct.kcasImpl
    bh.consume(s.michaelScottQueue1.enqueue.unsafePerform(ct.nextString()))
    bh.consume(s.transfer.unsafeRun)
    if (s.michaelScottQueue2.tryDeque.unsafeRun eq None) throw Errors.EmptyQueue
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  @Measurement(iterations = 20)
  def lockedQueue(s: LockedSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.lockedQueue1.enqueue(ct.nextString()))

    s.lockedQueue1.lock.lock()
    s.lockedQueue2.lock.lock()
    try {
      val item = s.lockedQueue1.unlockedTryDequeue().get
      bh.consume(s.lockedQueue2.unlockedEnqueue(item))
    } finally {
      s.lockedQueue1.lock.unlock()
      s.lockedQueue2.lock.unlock()
    }

    if (s.lockedQueue2.tryDequeue() eq None) throw Errors.EmptyQueue

    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def stmQueue(s: StmSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    import scala.concurrent.stm._
    bh.consume(s.stmQueue1.enqueue(ct.nextString()))
    bh.consume(atomic { implicit txn =>
      val item = s.stmQueue1.tryDequeue().get
      s.stmQueue2.enqueue(item)
    })
    if (s.stmQueue2.tryDequeue() eq None) throw Errors.EmptyQueue
    Blackhole.consumeCPU(ct.tokens)
  }
}

object QueueTransferBench {

  @State(Scope.Benchmark)
  class MsSt {
    val michaelScottQueue1 = new MichaelScottQueue[String](Prefill.prefill())
    val michaelScottQueue2 = new MichaelScottQueue[String](Prefill.prefill())
    val transfer = michaelScottQueue1.tryDeque.map(_.get) >>> michaelScottQueue2.enqueue
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedQueue1 = new LockedQueue[String](Prefill.prefill())
    val lockedQueue2 = new LockedQueue[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmQueue1 = new StmQueue[String](Prefill.prefill())
    val stmQueue2 = new StmQueue[String](Prefill.prefill())
  }
}