package com.example.rea
package bench
package util

import scala.concurrent.stm._

import StmQueue._

class StmQueue[A] private[this] (sentinel: Node[A]) {

  def this() =
    this(Node(nullOf[A], Ref(End[A]())))

  private[this] val head: Ref[Node[A]] =
    Ref(sentinel)

  private[this] val tail: Ref[Node[A]] =
    Ref(sentinel)

  def enqueue(a: A): Unit = atomic { implicit txn =>
    val node = Node(a, Ref[Elem[A]](End[A]()))
    tail.get.next.get match {
      case End() =>
        tail.get.next.set(node)
        tail.set(node)
      case Node(_, _) =>
        impossible("lagging tail")
    }
  }

  def tryDequeue(): Option[A] = atomic { implicit txn =>
    head.get.next.get match {
      case n @ Node(a, _) =>
        head.set(n.copy(data = nullOf[A]))
        Some(a)
      case End() =>
        None
    }
  }
}

object StmQueue {
  private sealed trait Elem[A]
  private final case class Node[A](data: A, next: Ref[Elem[A]]) extends Elem[A]
  private final case class End[A]() extends Elem[A]
}
