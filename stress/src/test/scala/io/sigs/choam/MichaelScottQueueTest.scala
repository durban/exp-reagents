package io.sigs.choam

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LLL_Result

import kcas.KCAS

@KCASParams("Treiber stack pop/push should be atomic")
@Outcomes(Array(
  new Outcome(id = Array("Some(z), Some(x), List(y)", "Some(z), None, List(x, y)"), expect = ACCEPTABLE, desc = "enq1 first; deq1 first"),
  new Outcome(id = Array("Some(x), Some(z), List(y)", "None, Some(z), List(x, y)"), expect = ACCEPTABLE, desc = "enq1 first; deq2 first"),
  new Outcome(id = Array("Some(z), Some(y), List(x)", "Some(z), None, List(y, x)"), expect = ACCEPTABLE, desc = "enq2 first; deq1 first"),
  new Outcome(id = Array("Some(y), Some(z), List(x)", "None, Some(z), List(y, x)"), expect = ACCEPTABLE, desc = "enq2 first; deq2 first"),
))
abstract class MichaelScottQueueTest(impl: KCAS) {

  private[this] val queue =
    new MichaelScottQueue[String](List("z"))

  private[this] val enqueue =
    queue.enqueue

  private[this] val tryDeque =
    queue.tryDeque

  @Actor
  def enq1(): Unit = {
    enqueue.unsafePerform("x")
  }

  @Actor
  def enq2(): Unit = {
    enqueue.unsafePerform("y")
  }

  @Actor
  def deq1(r: LLL_Result): Unit = {
    r.r1 = tryDeque.unsafeRun
  }

  @Actor
  def deq2(r: LLL_Result): Unit = {
    r.r2 = tryDeque.unsafeRun
  }

  @Arbiter
  def arbiter(r: LLL_Result): Unit = {
    r.r3 = queue.unsafeToList
  }
}
