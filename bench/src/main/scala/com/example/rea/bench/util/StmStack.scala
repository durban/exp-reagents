package com.example.rea
package bench
package util

import scala.concurrent.stm._

class StmStack[A](els: Iterable[A]) {

  def this() =
    this(Iterable.empty)

  private[this] val head: Ref[TsList[A]] =
    Ref(TsList.End)

  atomic { implicit txn =>
    els.foreach(push)
  }

  def push(a: A)(implicit mt: MaybeTxn): Unit = atomic { implicit txn =>
    head.set(TsList.Cons(a, head.get))
  }

  def tryPop()(implicit mt: MaybeTxn): Option[A] = atomic { implicit txn =>
    head.get match {
      case TsList.End =>
        None
      case TsList.Cons(h, t) =>
        head.set(t)
        Some(h)
    }
  }

  private[bench] def unsafeToList()(implicit mt: MaybeTxn): List[A] = atomic { implicit txn =>
    head.get.toList
  }
}

