package io.sigs.choam
package bench
package util

import scala.concurrent.stm._

import StmQueue._

class StmQueue[A] private[this] (sentinel: Node[A], els: Iterable[A]) {

  def this(els: Iterable[A]) =
    this(Node(nullOf[A], Ref(End[A]())), els)

  def this() =
    this(Iterable.empty)

  private[this] val head: Ref[Node[A]] =
    Ref(sentinel)

  private[this] val tail: Ref[Node[A]] =
    Ref(sentinel)

  atomic { implicit txn =>
    els.foreach(enqueue)
  }

  def enqueue(a: A)(implicit mt: MaybeTxn): Unit = atomic { implicit txn =>
    val node = Node(a, Ref[Elem[A]](End[A]()))
    tail.get.next.get match {
      case End() =>
        tail.get.next.set(node)
        tail.set(node)
      case Node(_, _) =>
        impossible("lagging tail")
    }
  }

  def tryDequeue()(implicit mt: MaybeTxn): Option[A] = atomic { implicit txn =>
    head.get.next.get match {
      case n @ Node(a, _) =>
        head.set(n.copy(data = nullOf[A]))
        Some(a)
      case End() =>
        None
    }
  }

  def unsafeToList()(implicit mt: MaybeTxn): List[A] = atomic { implicit txn =>
    @tailrec
    def go(e: Elem[A], acc: List[A]): List[A] = e match {
      case Node(null, next) =>
        // sentinel
        go(next.get, acc)
      case Node(a, next) =>
        go(next.get, a :: acc)
      case End() =>
        acc
    }

    go(head.get, Nil).reverse
  }
}

object StmQueue {
  private sealed trait Elem[A]
  private final case class Node[A](data: A, next: Ref[Elem[A]]) extends Elem[A]
  private final case class End[A]() extends Elem[A]
}
