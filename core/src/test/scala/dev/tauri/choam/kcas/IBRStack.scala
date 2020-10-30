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

import java.util.concurrent.atomic.AtomicInteger

import scala.annotation.tailrec

/** Treiber stack, which uses IBR for nodes */
final class IBRStack[A] {

  import IBRStack._

  private[this] val head =
    Ref.mk[Node[A]](End.widen)

  private[this] val _reusedCount =
    new AtomicInteger(0)

  /** For testing */
  private[kcas] def reusedCount: Int = {
    _reusedCount.get()
  }

  @tailrec
  def push(a: A): Unit = {
    val tc = threadContext[A]()
    tc.startOp()
    val done = try {
      val oldHead = tc.read(this.head)
      val newHead = tc.alloc(() => new Cons[A](nullOf[A], Ref.mk(nullOf[Node[A]])))
      if (newHead.freed > 0) {
        this._reusedCount.getAndIncrement()
      }
      newHead.asInstanceOf[Cons[A]].head = a
      tc.write(newHead.asInstanceOf[Cons[A]].tail, oldHead)
      tc.cas(this.head, oldHead, newHead)
    } finally tc.endOp() // FIXME: put the whole CAS-loop into one op?
    if (done) ()
    else push(a)
  }

  @tailrec
  def tryPop(): Option[A] = {
    val tc = threadContext[A]()
    tc.startOp()
    val res = try {
      val curr = tc.read(this.head)
      curr match {
        case c: Cons[_] =>
          val tail = tc.read(c.tail)
          if (tc.cas(this.head, curr, tail)) {
            tc.retire(curr)
            Some(c.head)
          } else {
            null // retry
          }
        case _ =>
          None
      }
    } finally tc.endOp()
    res match {
      case null => tryPop()
      case _ => res
    }
  }
}

final object IBRStack {

  private val gc = new IBR[Node[Any]](0L) {
    final override def dynamicTest[A](a: A): Boolean =
      a.isInstanceOf[Node[_]]
  }

  private def threadContext[A](): IBR.ThreadContext[Node[A]] =
    gc.threadContext().asInstanceOf[IBR.ThreadContext[Node[A]]]

  private[kcas] abstract class Node[A]
    extends DebugManaged[Node[A]]

  private[kcas] final class Cons[A](
    @volatile private[this] var _head: A,
    private[this] val _tail: Ref[Node[A]]
  ) extends Node[A] {

    def head: A = {
      this.checkAccess()
      this._head
    }

    def head_=(a: A): Unit = {
      this.checkAccess()
      this._head = a
    }

    def tail: Ref[Node[A]] = {
      this.checkAccess()
      this._tail
    }

    override protected[kcas] def allocate(tc: IBR.ThreadContext[Node[A]]): Unit = {
      super.allocate(tc)
      assert(equ(this._head, null))
      assert(equ(tc.read(this._tail), null))
    }

    override protected[kcas] def free(tc: IBR.ThreadContext[Node[A]]): Unit = {
      this._head = nullOf[A]
      tc.write(this._tail, nullOf[Node[A]])
      super.free(tc)
    }
  }

  private[kcas] final object End extends Node[Nothing] {
    @volatile private[this] var initialized = false
    override protected[kcas] def allocate(tc: IBR.ThreadContext[Node[Nothing]]): Unit = {
      super.allocate(tc)
      if (this.initialized) throw new Exception("End should never be reused")
      else this.initialized = true
    }
    override protected[kcas] def free(tc: IBR.ThreadContext[Node[Nothing]]): Unit = {
      super.free(tc)
      throw new Exception("End should never be freed")
    }
    def widen[A]: Node[A] =
      this.asInstanceOf[Node[A]]
  }
}
