/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
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

package io.sigs.choam
package kcas

import java.util.concurrent.atomic.AtomicReference
import java.util.concurrent.ThreadLocalRandom

/** k-CAS-able atomic reference */
sealed trait Ref[A] {

  final def upd[B, C](f: (A, B) => (A, C)): React[B, C] =
    React.upd(this)(f)

  final def updWith[B, C](f: (A, B) => React[Unit, (A, C)]): React[B, C] =
    React.updWith(this)(f)

  final def modify(f: A => A): React[Unit, A] =
    upd[Unit, A] { (a, _) => (f(a), a) }

  final def modifyWith(f: A => React[Unit, A]): React[Unit, A] =
    updWith[Unit, A] { (oa, _) => f(oa).map(na => (na, oa)) }

  private[choam] final val invisibleRead: React[Unit, A] =
    React.invisibleRead(this)

  final val getter: React[Unit, A] =
    upd[Unit, A] { (a, _) => (a, a) }

  private[choam] def cas(ov: A, nv: A): React[Unit, Unit] =
    React.cas(this, ov, nv)

  private[kcas] def unsafeTryRead(): A

  private[kcas] def unsafeTryPerformCas(ov: A, nv: A): Boolean

  private[kcas] def unsafeLazySet(nv: A): Unit

  private[kcas] def unsafeSet(nv: A): Unit

  private[kcas] def id0: Long

  private[kcas] def id1: Long

  private[kcas] def id2: Long

  private[kcas] def id3: Long

  @deprecated("don't use this, since it is terribly slow", since = "forever")
  private[kcas] final def bigId: BigInt = {
    val buf = java.nio.ByteBuffer.allocate(8 * 4)
    buf.putLong(this.id0)
    buf.putLong(this.id1)
    buf.putLong(this.id2)
    buf.putLong(this.id3)
    BigInt(buf.array())
  }

  private[kcas] def dummy(v: Long): Long

  final override def toString: String =
    s"Ref@${Integer.toHexString(this.##)}"

  final override def hashCode: Int =
    System.identityHashCode(this)

  final override def equals(that: Any): Boolean =
    equ(this, that)
}

object Ref {

  private[choam] def mk[A](a: A): Ref[A] = {
    val tlr = ThreadLocalRandom.current()
    new PaddedRefImpl(a)(tlr.nextLong(), tlr.nextLong(), tlr.nextLong(), tlr.nextLong())
  }

  /**
   * Only for testing
   *
   * TODO: provide unpadded groups of refs
   * (e.g., Ref2, Ref3) which still have
   * padding at the end.
   */
  private[kcas] def mkUnpadded[A](a: A): Ref[A] = {
    val tlr = ThreadLocalRandom.current()
    new UnpaddedRefImpl(a)(tlr.nextLong(), tlr.nextLong(), tlr.nextLong(), tlr.nextLong())
  }

  private[kcas] def globalCompare(a: Ref[_], b: Ref[_]): Int = {
    // TODO: try to optimize this comparison
    if (a eq b) 0
    else if (a.id0 > b.id0) 1
    else if (a.id0 < b.id0) -1
    else if (a.id1 > b.id1) 1
    else if (a.id1 < b.id1) -1
    else if (a.id2 > b.id2) 1
    else if (a.id2 < b.id2) -1
    else if (a.id3 > b.id3) 1
    else if (a.id3 < b.id3) -1
    else throw new IllegalStateException(s"[globalCompare] ref collision: ${a} and ${b}")
  }
}

private class UnpaddedRefImpl[A](initial: A)(i0: Long, i1: Long, i2: Long, i3: Long)
    extends AtomicReference[A](initial) with Ref[A] {

  private[kcas] final override val id0: Long = i0
  private[kcas] final override val id1: Long = i1
  private[kcas] final override val id2: Long = i2
  private[kcas] final override val id3: Long = i3

  private[kcas] final override def unsafeTryRead(): A =
    this.get()

  private[kcas] final override def unsafeSet(nv: A): Unit =
    this.set(nv)

  private[kcas] final override def unsafeLazySet(nv: A): Unit =
    this.lazySet(nv)

  private[kcas] final override def unsafeTryPerformCas(ov: A, nv: A): Boolean =
    this.compareAndSet(ov, nv)

  private[kcas] override def dummy(v: Long): Long =
    42L
}

private final class PaddedRefImpl[A](initial: A)(i0: Long, i1: Long, i2: Long, i3: Long)
    extends UnpaddedRefImpl[A](initial)(i0, i1, i2, i3) {

  @volatile private[this] var p00: Long = 42L
  @volatile private[this] var p01: Long = 42L
  @volatile private[this] var p02: Long = 42L
  @volatile private[this] var p03: Long = 42L
  @volatile private[this] var p04: Long = 42L
  @volatile private[this] var p05: Long = 42L
  @volatile private[this] var p06: Long = 42L
  @volatile private[this] var p07: Long = 42L
  @volatile private[this] var p08: Long = 42L
  @volatile private[this] var p09: Long = 42L
  @volatile private[this] var p10: Long = 42L
  @volatile private[this] var p11: Long = 42L
  @volatile private[this] var p12: Long = 42L
  @volatile private[this] var p13: Long = 42L
  @volatile private[this] var p14: Long = 42L

  private[kcas] final override def dummy(v: Long): Long = {
    p00 ^= v
    p01 ^= v
    p02 ^= v
    p03 ^= v
    p04 ^= v
    p05 ^= v
    p06 ^= v
    p07 ^= v
    p08 ^= v
    p09 ^= v
    p10 ^= v
    p11 ^= v
    p12 ^= v
    p13 ^= v
    p14 ^= v
    42L
  }
}
