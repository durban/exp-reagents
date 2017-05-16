package com.example.rea
package bench

import org.openjdk.jmh.annotations.Benchmark
import org.openjdk.jmh.infra.Blackhole

import util._

class BaselineBench {

  @Benchmark
  def baseline(t: CommonThreadState): Unit = {
    Blackhole.consumeCPU(t.tokens)
  }

  @Benchmark
  def baseline2(t: CommonThreadState): Unit = {
    Blackhole.consumeCPU(t.halfTokens)
    Blackhole.consumeCPU(t.halfTokens)
  }
}
