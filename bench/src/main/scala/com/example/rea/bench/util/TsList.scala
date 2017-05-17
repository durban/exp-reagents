package com.example.rea
package bench
package util

/** Thread-safe list */
private[bench] sealed abstract class TsList[+A] {

  import TsList._

  def length: Int = {
    @tailrec
    def go(l: TsList[A], acc: Int): Int = l match {
      case End => acc
      case Cons(_, t) => go(t, acc + 1)
    }
    go(this, 0)
  }

  def toList: List[A] = {
    val b = new scala.collection.mutable.ListBuffer[A]
    @tailrec
    def go(l: TsList[A]): Unit = l match {
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

private[bench] object TsList {
  final case class Cons[A](h: A, t: TsList[A]) extends TsList[A]
  final case object End extends TsList[Nothing]
}
