package io.sigs.choam
package bench

import org.openjdk.jmh.annotations._

import kcas._
import util._

@Fork(2)
@Warmup(iterations = 10)
@Measurement(iterations = 10)
class CombinatorsBench {

  import CombinatorsBench._

  @Benchmark
  def dummyChoice(s: DummyChoice, k: KCASImplStateImpl): Unit = {
    import k.kcasImpl
    s.choice.unsafeRun
    s.reset.unsafeRun
  }

  @Benchmark
  def casChoice(s: CASChoice, k: KCASImplStateImpl): Unit = {
    import k.kcasImpl
    s.choice.unsafeRun
    s.reset.unsafeRun
  }
}

object CombinatorsBench {

  @State(Scope.Thread)
  class KCASImplStateImpl extends KCASImplState

  @State(Scope.Thread)
  class DummyChoice {

    private[this] val ref =
      Ref.mk("foo")

    @Param(Array("8", "16", "32"/*, "64", "128"*/))
    var size: Int = _

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
  class CASChoice {

    private[this] val ref =
      Ref.mk("foo")

    private[this] var refs: Array[Ref[String]] =
      _

    @Param(Array("8", "16", "32"/*, "64", "128"*/))
    var size: Int = _

    val reset: React[Unit, Unit] =
      ref.modify(_ => "foo").discard

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
