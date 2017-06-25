package io.sigs.choam

import kcas._

final class TreiberStack[A](els: Iterable[A]) {

  import TreiberStack._

  def this() =
    this(Iterable.empty)

  private[this] val head = Ref.mk[Lst[A]](End)

  val push: React[A, Unit] = head.upd { (as, a) =>
    (Cons(a, as), ())
  }

  val tryPop: React[Unit, Option[A]] = head.upd {
    case (Cons(h, t), ()) => (t, Some(h))
    case (End, ()) => (End, None)
  }

  val length: React[Unit, Int] =
    head.upd[Unit, Int] { (l, _) => (l, l.length) }

  private[choam] def unsafeToList(implicit kcas: KCAS): List[A] = {
    val r = head.upd[Unit, Lst[A]] { (l, _) => (l, l) }
    r.unsafeRun.toList
  }

  els.foreach { a =>
    push.unsafePerform(a)(KCAS.NaiveKCAS)
  }
}

private object TreiberStack {

  private sealed trait Lst[+A] {

    def length: Int = {
      @tailrec
      def go(l: Lst[A], acc: Int): Int = l match {
        case End => acc
        case Cons(_, t) => go(t, acc + 1)
      }
      go(this, 0)
    }

    def toList: List[A] = {
      val b = new scala.collection.mutable.ListBuffer[A]
      @tailrec
      def go(l: Lst[A]): Unit = l match {
        case End =>
          ()
        case Cons(h, t) =>
          b += h
          go(t)
      }
      go(this)
      b.toList
    }
  }

  private final case class Cons[A](h: A, t: Lst[A]) extends Lst[A]

  private final case object End extends Lst[Nothing]
}
