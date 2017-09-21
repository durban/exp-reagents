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
package kcas
package bench

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import io.sigs.choam.bench.util.RandomState

@Fork(4)
@Warmup(iterations = 10)
@Measurement(iterations = 20)
class FalseSharing {

  import FalseSharing._

  @Benchmark
  @Group("baseline")
  def readBaseline(s: NotContended, bh: Blackhole): Unit = {
    bh.consume(s.rr)
  }

  @Benchmark
  @Group("baseline")
  def writeBaseline(s: NotContended, r: RandomState): Unit = {
    s.rw = box(r.nextInt())
  }

  @Benchmark
  @Group("unpadded")
  def readUnpadded(s: Unpadded, bh: Blackhole): Unit = {
    bh.consume(s.rr.unsafeTryRead())
  }

  @Benchmark
  @Group("unpadded")
  def writeUnpadded(s: Unpadded, r: RandomState): Unit = {
    s.rw.unsafeSet(r.nextInt())
  }

  @Benchmark
  @Group("padded")
  def readPadded(s: Padded, bh: Blackhole): Unit = {
    bh.consume(s.rr.unsafeTryRead())
  }

  @Benchmark
  @Group("padded")
  def writePadded(s: Padded, r: RandomState): Unit = {
    s.rw.unsafeSet(r.nextInt())
  }
}

object FalseSharing {

  @State(Scope.Thread)
  class NotContended {
    var rr: AnyRef = box(42)
    var rw: AnyRef = box(21)
  }

  abstract class Base {

    def rr: Ref[Int]
    def rw: Ref[Int]

    @TearDown
    def checkResults(): Unit = {
      rr.unsafeTryRead() match {
        case 42 => // OK
        case x => throw new IllegalStateException(s"unexpected value in rr: '${x}'")
      }
    }
  }

  @State(Scope.Benchmark)
  class Unpadded extends Base {
    final override val rr: Ref[Int] = Ref.mkUnpadded(42)
    final override val rw: Ref[Int] = Ref.mkUnpadded(21)
  }

  @State(Scope.Benchmark)
  class Padded extends Base {
    final override val rr: Ref[Int] = Ref.mk(42)
    final override val rw: Ref[Int] = Ref.mk(21)
  }
}
