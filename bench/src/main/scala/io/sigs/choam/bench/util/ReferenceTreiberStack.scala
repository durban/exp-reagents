package io.sigs.choam
package bench
package util

import java.util.concurrent.atomic.AtomicReference

final class ReferenceTreiberStack[A](els: Iterable[A]) {

  def this() =
    this(Iterable.empty)

  private[this] val head =
    new AtomicReference[TsList[A]](TsList.End)

  els.foreach(push)

  @tailrec
  def push(a: A): Unit = {
    val curr = head.get()
    if (head.compareAndSet(curr, TsList.Cons(a, curr))) ()
    else push(a)
  }

  @tailrec
  def tryPop(): Option[A] = {
    val curr = head.get()
    curr match {
      case TsList.End =>
        None
      case TsList.Cons(h, t) =>
        if (head.compareAndSet(curr, t)) Some(h)
        else tryPop()
    }
  }

  def length: Int =
    head.get().length
}
