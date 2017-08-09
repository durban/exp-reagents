package io.sigs.choam
package bench

import org.openjdk.jmh.annotations._

import util._
import kcas._
import kcas.bench.Reset

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ProductCombinatorBench {

  import ProductCombinatorBench._

  @Benchmark
  def productDummy(s: DummyProduct, k: KCASImplStateImpl): Unit = {
    import k.kcasImpl
    s.prod.unsafeRun
  }

  @Benchmark
  def productCAS(s: CASProduct, k: KCASImplStateImpl): Unit = {
    import k.kcasImpl
    s.prod.unsafeRun
    s.reset.reset()
  }
}

object ProductCombinatorBench {

  @State(Scope.Thread)
  class DummyProduct extends ChoiceCombinatorBench.BaseState {

    var prod: React[Unit, Unit] = _

    @Setup
    def setup(): Unit = {
      this.prod = (1 to size).foldLeft[React[Unit, Unit]](React.ret(())) { (r, idx) =>
        (r * React.lift[String, String](_ + idx.toString).lmap[Unit](_ => "foo")).discard
      }
    }
  }

  @State(Scope.Thread)
  class CASProduct extends ChoiceCombinatorBench.BaseState {

    var prod: React[Unit, Unit] = _

    private[this] var refs: Array[Ref[String]] = _

    var reset: Reset[String] = _

    @Setup
    def setup(): Unit = {
      this.refs = Array.fill(size)(Ref.mk("foo"))
      this.reset = new Reset("foo", this.refs: _*)
      this.prod = (0 until size).foldLeft[React[Unit, Unit]](React.ret(())) { (r, idx) =>
        (r * refs(idx).cas("foo", "bar")).discard
      }
    }
  }
}
