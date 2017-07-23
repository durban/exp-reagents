package io.sigs.choam.bench

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole

import io.sigs.choam.bench.util.{ CommonThreadState, RandomState, KCASImplState }

import io.sigs.choam._
import io.sigs.choam.kcas.{ Ref, KCAS }

/**
 * A variant of `io.sigs.choam.kcas.bench.ResourceAllocation`,
 * implemented with reagents.
 */
@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ResourceAllocationReact {

  import ResourceAllocationReact._

  @Benchmark
  def bench(s: ResAllocSt, t: ThreadSt): Unit = {
    val n = t.allocSize
    implicit val kcas: KCAS = t.kcasImpl
    val rss = t.selectResources(s.rss)

    @tailrec
    def read(i: Int, react: React[Unit, Vector[String]]): React[Unit, Vector[String]] = {
      if (i >= n) {
        react
      } else {
        val r = rss(i).read
        read(i + 1, react.map2(r)(_ :+ _))
      }
    }

    @tailrec
    def write(i: Int, react: React[Vector[String], Unit]): React[Vector[String], Unit] = {
      if (i >= n) {
        react
      } else {
        val r = React.computed[Vector[String], Unit] { ovs =>
          rss(i).cas(ovs(i), ovs((i + 1) % n))
        }
        write(i + 1, (react * r).discard)
      }
    }

    val r = read(0, React.ret(Vector.empty))
    val w = write(0, React.unit)
    (r >>> w).unsafeRun

    Blackhole.consumeCPU(t.tokens)
  }
}

object ResourceAllocationReact {

  private[this] final val nRes = 60

  @State(Scope.Benchmark)
  class ResAllocSt {

    private[this] val initialValues =
      Vector.fill(nRes)(scala.util.Random.nextString(10))

    val rss: Array[Ref[String]] =
      initialValues.map(Ref.mk).toArray

    @TearDown
    def checkResults(): Unit = {
      val currentValues = rss.map(_.read.unsafeRun(KCAS.NaiveKCAS)).toVector
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
  class ThreadSt extends RandomState with KCASImplState {

    val tokens: Long =
      CommonThreadState.BaseTokens << CommonThreadState.LowContention

    private[this] var selectedRss: Array[Ref[String]] = _

    @Param(Array("2", "4", "6"))
    private[this] var dAllocSize: Int = _

    def allocSize: Int =
      dAllocSize

    @Setup
    def setupSelRes(): Unit = {
      selectedRss = Array.ofDim(allocSize)
    }

    /** Select `allocSize` refs randomly */
    def selectResources(rss: Array[Ref[String]]): Array[Ref[String]] = {
      val bucketSize = nRes / allocSize

      @tailrec
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
