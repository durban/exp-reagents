/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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
package kcas
package bench

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import dev.tauri.choam.bench.util.{ RandomState, XorShift, ReferenceTreiberStack, TsList }
import dev.tauri.choam.kcas.IBRStackFast

@Fork(2)
class IBRBench {

  import IBRBench._

  @Benchmark
  def stackBaseline(s: BaselineStackSt, t: ThSt, bh: Blackhole): Unit = {
    bh.consume(s.stack.push(t.nextInt()))
    assert(s.stack.tryPop().isDefined)
  }

  @Benchmark
  def stackIbr(s: StackSt, t: ThSt, bh: Blackhole): Unit = {
    bh.consume(s.stack.push(t.nextInt(), t.tc))
    assert(s.stack.tryPop(t.tc) ne null)
  }

  @Benchmark
  def allocBaseline(t: ThSt, bh: Blackhole): Unit = {
    bh.consume(TsList.Cons[Int](42, t.dummy))
  }

  @Benchmark
  def allocIbr(t: ThSt, bh: Blackhole): Unit = {
    bh.consume(t.tc.alloc())
  }

  // TODO: add a benchmark with a `kcas.Ref`-based stack
}

object IBRBench {

  @State(Scope.Benchmark)
  class StackSt {
    val stack = {
      val xs = XorShift()
      IBRStackFast[Int](List.fill(100) { xs.nextInt() }: _*)
    }
  }

  @State(Scope.Benchmark)
  class BaselineStackSt {
    val stack = {
      val xs = XorShift()
      new ReferenceTreiberStack[Int](List.fill(100) { xs.nextInt() })
    }
  }

  @State(Scope.Thread)
  class ThSt extends RandomState {
    val tc = IBRStackFast.threadLocalContext[Int]()
    val dummy = TsList.Cons(42, null)
  }
}
