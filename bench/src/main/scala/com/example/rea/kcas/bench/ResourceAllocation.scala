package com.example.rea
package kcas
package bench

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import com.example.rea.bench.util.{ CommonThreadState, RandomState, KCASImplState }

/**
 * Resource allocation scenario, described in [Software transactional memory](
 * https://pdfs.semanticscholar.org/846e/87f6c8b9d8909d678b5c668cfe46cf40a348.pdf)
 * by Nir Shavit and Dan Touitou.
 */
@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ResourceAllocation {

  import ResourceAllocation._

  @Benchmark
  def bench(s: RaSt, t: ThSt): Unit = {
    val n = t.allocSize
    val impl = t.kcasImpl
    val rss = t.selectResources(s.rss)
    val ovs = t.ovs
    def read(i: Int): Unit = {
      if (i >= n) {
        ()
      } else {
        ovs(i) = impl.read(rss(i))
        read(i + 1)
      }
    }
    def prepare(i: Int, d: impl.Desc): impl.Desc = {
      if (i >= n) {
        d
      } else {
        val nd = d.withCAS(rss(i), ovs(i), ovs((i + 1) % n))
        prepare(i + 1, nd)
      }
    }
    def go(): Unit = {
      read(0)
      val d = prepare(0, impl.start())
      if (d.tryPerform()) ()
      else go()
    }

    go()
    Blackhole.consumeCPU(t.tokens)
  }
}

object ResourceAllocation {

  private[this] final val nRes = 60

  @State(Scope.Benchmark)
  class RaSt {

    private[this] val initialValues =
      Vector.fill(nRes)(scala.util.Random.nextString(10))

    val rss: Array[Ref[String]] =
      initialValues.map(Ref.mk).toArray

    @TearDown
    def checkResults(): Unit = {
      val currentValues = rss.map(_.unsafeTryRead()).toVector
      if (currentValues == initialValues) {
        throw new Exception(s"Unchanged results")
      }
      val cv = currentValues.sorted
      val iv = initialValues.sorted
      if (cv != iv) {
        throw new Exception(s"Invalid results: ${cv} != ${iv}")
      }
    }
  }

  @State(Scope.Thread)
  class ThSt extends RandomState with KCASImplState {

    val tokens: Long =
      CommonThreadState.BaseTokens << CommonThreadState.LowContention

    private[this] var selectedRss: Array[Ref[String]] = _

    var ovs: Array[String] = _

    @Param(Array("2", "4", "6"))
    private[this] var dAllocSize: Int = _

    def allocSize: Int =
      dAllocSize

    @Setup
    def setupSelRes(): Unit = {
      selectedRss = Array.ofDim(allocSize)
      ovs = Array.ofDim(allocSize)
    }

    /** Select `allocSize` refs randomly */
    def selectResources(rss: Array[Ref[String]]): Array[Ref[String]] = {
      val bucketSize = nRes / allocSize
      def next(off: Int, dest: Int): Unit = {
        if (dest >= allocSize) {
          ()
        } else {
          val rnd = (nextInt() % bucketSize).abs
          selectedRss(dest) = rss(off + rnd)
          next(off + bucketSize, dest + 1)
        }
      }

      next(0, 0)
      selectedRss
    }
  }
}