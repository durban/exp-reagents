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

import org.openjdk.jmh.annotations._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
@BenchmarkMode(Array(Mode.AverageTime))
class BackoffBench {

  import BackoffBench._

  @Benchmark
  def backoff(bo: BoSt): Unit = {
    bo.b.backoffFix(bo.w)
  }
}

object BackoffBench {

  @State(Scope.Thread)
  class BoSt {

    val b = new Backoff

    @Param(Array("1", "2", "4", "8"))
    var w: Int = _
  }
}
