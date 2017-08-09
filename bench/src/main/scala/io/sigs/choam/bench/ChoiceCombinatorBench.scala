package io.sigs.choam
package bench

import org.openjdk.jmh.annotations._

import kcas._
import kcas.bench.Reset
import util._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class ChoiceCombinatorBench {

  import ChoiceCombinatorBench._

  @Benchmark
  def choiceDummy(s: DummyChoice, k: KCASImplStateImpl): Unit = {
    import k.kcasImpl
    s.choice.unsafeRun
    s.reset.unsafeRun
  }

  @Benchmark
  def choiceCAS(s: CASChoice, k: KCASImplStateImpl): Unit = {
    import k.kcasImpl
    s.choice.unsafeRun
    s.reset.reset()
  }
}

object ChoiceCombinatorBench {

  @State(Scope.Thread)
  abstract class BaseState {
    @Param(Array("8", "16", "32"))
    var size: Int = _
  }

  @State(Scope.Thread)
  class DummyChoice extends BaseState {

    private[this] val ref =
      Ref.mk("foo")

    val reset: React[Unit, Unit] =
      ref.modify(_ => "foo").discard

    var choice: React[Unit, Unit] = _

    def mkChoice(n: Int): React[Unit, Unit] = {
      val successfulCas = ref.cas("foo", "bar")
      val fails = (1 to n).foldLeft[React[Unit, Unit]](React.retry) { (r, _) =>
        r + React.retry
      }
      fails + successfulCas
    }

    @Setup
    def setup(): Unit = {
      this.choice = mkChoice(size)
    }
  }

  @State(Scope.Thread)
  class CASChoice extends BaseState {

    private[this] val ref =
      Ref.mk("foo")

    private[this] var refs: Array[Ref[String]] =
      _

    val reset: Reset[String] =
      new Reset("foo", ref)

    var choice: React[Unit, Unit] = _

    def mkChoice(): React[Unit, Unit] = {
      val successfulCas = ref.cas("foo", "bar")
      val fails = refs.foldLeft[React[Unit, Unit]](React.retry) { (r, ref) =>
        r + ref.cas("invalid", "dontcare")
      }
      fails + successfulCas
    }

    @Setup
    def setup(): Unit = {
      this.refs = Array.tabulate(size)(i => Ref.mk(i.toString))
      this.choice = mkChoice()
    }
  }
}
