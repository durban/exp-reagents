package com.example.rea
package bench
package util

import java.util.concurrent.atomic.AtomicReference

final class ReferenceTreiberStack[A](els: Iterable[A]) {

  import ReferenceTreiberStack._

  def this() =
    this(Iterable.empty)

  private[this] val head =
    new AtomicReference[Lst[A]](End)

  els.foreach(push)

  @tailrec
  def push(a: A): Unit = {
    val curr = head.get()
    if (head.compareAndSet(curr, Cons(a, curr))) ()
    else push(a)
  }

  @tailrec
  def tryPop(): Option[A] = {
    val curr = head.get()
    curr match {
      case End =>
        None
      case Cons(h, t) =>
        if (head.compareAndSet(curr, t)) Some(h)
        else tryPop()
    }
  }

  def length: Int = {
    @tailrec
    def go(l: Lst[A], acc: Int): Int = l match {
      case End => acc
      case Cons(_, t) => go(t, acc + 1)
    }
    go(head.get(), 0)
  }
}

object ReferenceTreiberStack {
  private sealed trait Lst[+A]
  private final case class Cons[A](h: A, t: Lst[A]) extends Lst[A]
  private final case object End extends Lst[Nothing]
}
