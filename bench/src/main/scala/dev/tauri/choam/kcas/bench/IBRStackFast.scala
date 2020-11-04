/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dev.tauri.choam
package kcas
package bench

import scala.annotation.tailrec

import cats.data.Chain

/** Treiber stack, which uses IBR for nodes, and has no debug assertions */
final class IBRStackFast[A] private (val gc: IBR[IBRStackFast.Node[A]]) {

  import IBRStackFast.{ Node, Cons, End }

  private[this] val head =
    Ref.mk[Node[A]](End.widen)

  private[this] val _sentinel: A =
    (new AnyRef).asInstanceOf[A]

  @tailrec
  def push(a: A, tc: IBR.ThreadContext[Node[A]]): Unit = {
    tc.startOp()
    val done = try {
      val oldHead = tc.read(this.head)
      val newHead = tc.alloc()
      newHead.asInstanceOf[Cons[A]].head = a
      tc.write(newHead.asInstanceOf[Cons[A]].tail, oldHead)
      if (tc.cas(this.head, oldHead, newHead)) {
        true
      } else {
        tc.retire(newHead)
        false
      }
    } finally tc.endOp() // FIXME: put the whole CAS-loop into one op?
    if (!done) {
      push(a, tc) // retry
    }
  }

  @tailrec
  def tryPop(tc: IBR.ThreadContext[Node[A]]): A = {
    tc.startOp()
    val res = try {
      val curr = tc.read(this.head)
      curr match {
        case c: Cons[_] =>
          val tail = tc.read(c.tail)
          if (tc.cas(this.head, curr, tail)) {
            tc.retire(curr)
            c.head
          } else {
            _sentinel // retry
          }
        case _ =>
          nullOf[A]
      }
    } finally tc.endOp()
    if (equ(res, _sentinel)) tryPop(tc)
    else res
  }

  /** For testing; not threadsafe */
  private[kcas] def unsafeToList(tc: IBR.ThreadContext[Node[A]]): List[A] = {
    @tailrec
    def go(next: Ref[Node[A]], acc: Chain[A]): Chain[A] = {
      tc.read(next) match {
        case c: Cons[_] =>
          go(c.tail, acc :+ c.head)
        case _: End.type =>
          acc
      }
    }
    tc.startOp()
    try {
      go(this.head, Chain.empty).toList
    } finally tc.endOp()
  }
}

final object IBRStackFast {

  def apply[A](els: A*): IBRStackFast[A] = {
    val s = new IBRStackFast[A](gc.asInstanceOf[IBR[Node[A]]])
    val tc = s.gc.threadContext()
    els.foreach(e => s.push(e, tc))
    s
  }

  private[kcas] def threadLocalContext[A](): IBR.ThreadContext[Node[A]] =
    gc.asInstanceOf[IBR[Node[A]]].threadContext()

  private[this] val gc: IBR[Node[Any]] = new IBR[Node[Any]](0L) {
    final override def allocateNew(): Node[Any] =
      new Cons[Any](nullOf[Any], Ref.mk(nullOf[Node[Any]]))
    final override def dynamicTest[X](a: X): Boolean =
      a.isInstanceOf[Node[_]]
  }

  private[kcas] abstract class Node[A]
    extends IBR.Managed[Node[A]]

  private[kcas] final class Cons[A](
    @volatile private[kcas] var head: A,
    private[kcas] val tail: Ref[Node[A]]
  ) extends Node[A] {

    override protected[kcas] def allocate(tc: IBR.ThreadContext[Node[A]]): Unit =
      ()

    override protected[kcas] def free(tc: IBR.ThreadContext[Node[A]]): Unit = {
      this.head = nullOf[A]
      tc.write(this.tail, nullOf[Node[A]])
    }
  }

  private[kcas] final object End extends Node[Nothing] {
    override protected[kcas] def allocate(tc: IBR.ThreadContext[Node[Nothing]]): Unit =
      ()
    override protected[kcas] def free(tc: IBR.ThreadContext[Node[Nothing]]): Unit =
      throw new Exception("End should never be freed")
    def widen[A]: Node[A] =
      this.asInstanceOf[Node[A]]
  }
}
