package com.example.rea

import kcas._

import MichaelScottQueue._

final class MichaelScottQueue[A] private[this] (sentinel: Node[A]) {

  private[this] val head: Ref[Node[A]] = Ref.mk(sentinel)
  private[this] val tail: Ref[Node[A]] = Ref.mk(sentinel)

  def this() =
    this(Node(nullOf[A], Ref.mk(End[A]())))

  val tryDeque: React[Unit, Option[A]] = {
    for {
      node <- head.read
      next <- node.next.read
      res <- next match {
        case n @ Node(a, _) =>
          head.cas(node, n.copy(data = nullOf[A])) >>> React.ret(Some(a))
        case End() =>
          head.cas(node, node) >>> React.ret(None)
      }
    } yield res
  }

  val enqueue: React[A, Unit] = React.computed { a: A =>
    findAndEnqueue(Node(a, Ref.mk(End[A]())))
  }

  private[this] def findAndEnqueue(node: Node[A]): React[Unit, Unit] = {
    tail.read.postCommit(React.computed { n: Node[A] =>
      n.next.read.flatMap {
        case e @ End() =>
          // found true tail; CAS, and try to adjust the tail ref:
          n.next.cas(e, node).postCommit(tail.cas(n, node).?.rmap(_ => ()))
        case nv @ Node(_, _) =>
          // not the true tail; try to catch up, and retry:
          tail.cas(n, nv).?.postCommit(React.computed(_ => findAndEnqueue(node))).rmap(_ => ())
      }
    }).rmap(_ => ())
  }

  private[rea] def unsafeToList(implicit kcas: KCAS): List[A] = {
    @tailrec
    def go(e: Elem[A], acc: List[A]): List[A] = e match {
      case Node(null, next) =>
        // sentinel
        go(next.read.unsafeRun, acc)
      case Node(a, next) =>
        go(next.read.unsafeRun, a :: acc)
      case End() =>
        acc
    }

    go(head.read.unsafeRun, Nil).reverse
  }
}

object MichaelScottQueue {

  private sealed trait Elem[A]
  private final case class Node[A](data: A, next: Ref[Elem[A]]) extends Elem[A]
  private final case class End[A]() extends Elem[A]
}
