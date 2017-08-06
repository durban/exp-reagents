package io.sigs.choam

import kcas._

import MichaelScottQueue._

final class MichaelScottQueue[A] private[this] (sentinel: Node[A], els: Iterable[A]) {

  private[this] val head: Ref[Node[A]] = Ref.mk(sentinel)
  private[this] val tail: Ref[Node[A]] = Ref.mk(sentinel)

  def this(els: Iterable[A]) =
    this(Node(nullOf[A], Ref.mk(End[A]())), els)

  def this() =
    this(Iterable.empty)

  val tryDeque: React[Unit, Option[A]] = {
    for {
      node <- head.invisibleRead
      next <- node.next.invisibleRead
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
    tail.invisibleRead.postCommit(React.computed { n: Node[A] =>
      n.next.invisibleRead.flatMap {
        case e @ End() =>
          // found true tail; CAS, and try to adjust the tail ref:
          n.next.cas(e, node).postCommit(tail.cas(n, node).?.rmap(_ => ()))
        case nv @ Node(_, _) =>
          // not the true tail; try to catch up, and retry:
          tail.cas(n, nv).?.postCommit(React.computed(_ => findAndEnqueue(node))).rmap(_ => ())
      }
    }).rmap(_ => ())
  }

  private[choam] def unsafeToList(implicit kcas: KCAS): List[A] = {
    @tailrec
    def go(e: Elem[A], acc: List[A]): List[A] = e match {
      case Node(null, next) =>
        // sentinel
        go(next.invisibleRead.unsafeRun, acc)
      case Node(a, next) =>
        go(next.invisibleRead.unsafeRun, a :: acc)
      case End() =>
        acc
    }

    go(head.invisibleRead.unsafeRun, Nil).reverse
  }

  els.foreach { a =>
    enqueue.unsafePerform(a)(KCAS.NaiveKCAS)
  }
}

object MichaelScottQueue {

  private sealed trait Elem[A]
  private final case class Node[A](data: A, next: Ref[Elem[A]]) extends Elem[A]
  private final case class End[A]() extends Elem[A]
}
