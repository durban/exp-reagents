package com.example.rea
package bench
package util

import java.util.concurrent.atomic.AtomicReference

final class ReferenceTreiberStack[A](els: Iterable[A]) {

  def this() =
    this(Iterable.empty)

  private[this] val head =
    new AtomicReference[List[A]](Nil)

  els.foreach(push)

  @tailrec
  def push(a: A): Unit = {
    val curr = head.get()
    if (head.compareAndSet(curr, a :: curr)) ()
    else push(a)
  }

  @tailrec
  def tryPop(): Option[A] = {
    val curr = head.get()
    curr match {
      case Nil =>
        None
      case h :: t =>
        if (head.compareAndSet(curr, t)) Some(h)
        else tryPop()
    }
  }

  def length: Int =
    head.get().length
}
