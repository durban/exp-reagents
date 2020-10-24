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

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import util._

@Fork(2)
class StackTransferBench {

  import StackTransferBench._

  @Benchmark
  def treiberStack(s: TreiberSt, bh: Blackhole, ct: KCASThreadState): Unit = {
    import ct.kcasImpl
    bh.consume(s.treiberStack1.push.unsafePerform(ct.nextString()))
    bh.consume(s.transfer.unsafeRun)
    if (s.treiberStack2.tryPop.unsafeRun eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def lockedStack(s: LockedSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    bh.consume(s.lockedStack1.push(ct.nextString()))

    s.lockedStack1.lock.lock()
    s.lockedStack2.lock.lock()
    try {
      val item = s.lockedStack1.unlockedTryPop().get
      bh.consume(s.lockedStack2.unlockedPush(item))
    } finally {
      s.lockedStack1.lock.unlock()
      s.lockedStack2.lock.unlock()
    }

    if (s.lockedStack2.tryPop() eq None) throw Errors.EmptyStack

    Blackhole.consumeCPU(ct.tokens)
  }

  @Benchmark
  def stmStack(s: StmSt, bh: Blackhole, ct: CommonThreadState): Unit = {
    import scala.concurrent.stm._
    bh.consume(s.stmStack1.push(ct.nextString()))
    bh.consume(atomic { implicit txn =>
      val item = s.stmStack1.tryPop().get
      s.stmStack2.push(item)
    })
    if (s.stmStack2.tryPop() eq None) throw Errors.EmptyStack
    Blackhole.consumeCPU(ct.tokens)
  }
}

object StackTransferBench {

  @State(Scope.Benchmark)
  class TreiberSt {
    val treiberStack1 =
      new TreiberStack[String](Prefill.prefill())
    val treiberStack2 =
      new TreiberStack[String](Prefill.prefill())
    val transfer =
      treiberStack1.tryPop.map(_.get) >>> treiberStack2.push
  }

  @State(Scope.Benchmark)
  class LockedSt {
    val lockedStack1 =
      new LockedStack[String](Prefill.prefill())
    val lockedStack2 =
      new LockedStack[String](Prefill.prefill())
  }

  @State(Scope.Benchmark)
  class StmSt {
    val stmStack1 =
      new StmStack[String](Prefill.prefill())
    val stmStack2 =
      new StmStack[String](Prefill.prefill())
  }
}
