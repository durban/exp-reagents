package com.example.rea
package bench
package util

import scala.concurrent.stm._

class StmStack[A] {

  private[this] val head: Ref[List[A]] =
    Ref(Nil)

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
