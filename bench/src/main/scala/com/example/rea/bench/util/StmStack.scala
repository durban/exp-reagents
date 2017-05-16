package com.example.rea
package bench
package util

import scala.concurrent.stm._

class StmStack[A](els: Iterable[A]) {

  def this() =
    this(Iterable.empty)

  private[this] val head: Ref[List[A]] =
    Ref(Nil)

  atomic { implicit txn =>
    els.foreach(push)
  }

  def push(a: A): Unit = atomic { implicit txn =>
    head.set(a :: head.get)
  }

  def tryPop(): Option[A] = atomic { implicit txn =>
    head.get match {
      case Nil =>
        None
      case h :: t =>
        head.set(t)
        Some(h)
    }
  }

  private[bench] def unsafeToList(): List[A] = atomic { implicit txn =>
    head.get
  }
}
