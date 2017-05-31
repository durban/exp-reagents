package com.example.rea
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
