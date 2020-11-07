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

import java.util.concurrent.atomic.AtomicLong

import IBRStackFast.{ Node, Cons, TC }

/** Treiber stack, which uses IBR for nodes, and has extra verification checks */
final class IBRStackDebug[A] private (val debugGc: IBRStackDebug.GC[A])
  extends IBRStackFast[A](debugGc) {

  final override def checkReuse(tc: TC[A], c: IBRStackFast.Cons[A]): Unit = {
    if (c.asInstanceOf[DebugManaged[TC[A], Node[A]]].freed > 0) {
      tc.globalContext.asInstanceOf[IBRStackDebug.GC[A]].reuseCount.getAndIncrement()
      ()
    }
  }
}

final object IBRStackDebug {

  def apply[A](els: A*): IBRStackDebug[A] = {
    val s = new IBRStackDebug[A](gc.asInstanceOf[GC[A]])
    val tc = s.gc.threadContext()
    els.foreach(e => s.push(e, tc))
    s
  }

  private[kcas] def threadLocalContext[A](): TC[A] =
    gc.asInstanceOf[IBR[TC[A], Node[A]]].threadContext()

  private[this] val gc =
    new GC[Any]

  final class GC[A] extends IBRStackFast.GC[A] {

    val reuseCount = new AtomicLong

    final override def allocateNew() = {
      new DebugCons[A]
    }
  }

  final class DebugCons[A] extends Cons[A] with DebugManaged[TC[A], Node[A]] {
    override def allocate(tc: TC[A]): Unit = {
      super.allocate(tc)
      val hd = this.getHeadVh().get(this)
      assert(hd eq null, s"head is '${hd}'")
      assert(equ(tc.readVh[Node[A]](this.getTailVh(), this), null))
    }
    override def free(tc: TC[A]): Unit = {
      super[DebugManaged].free(tc)
      super[Cons].free(tc)
    }
    override def getHeadVh() = {
      this.checkAccess()
      super.getHeadVh()
    }
    override def getTailVh() = {
      this.checkAccess()
      super.getTailVh()
    }
  }
}
