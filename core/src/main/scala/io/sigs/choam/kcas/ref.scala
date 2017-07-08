package io.sigs.choam
package kcas

import java.util.concurrent.atomic.AtomicReference

/** k-CAS-able atomic reference */
sealed trait Ref[A] {

  final def upd[B, C](f: (A, B) => (A, C)): React[B, C] =
    React.upd(this)(f)

  final def modify(f: A => A): React[Unit, A] =
    upd[Unit, A] { (a, _) => (f(a), a) }

  final val getter: React[Unit, A] =
    upd[Unit, A] { (a, _) => (a, a) }

  private[choam] def cas(ov: A, nv: A): React[Unit, Unit] =
    React.cas(this, ov, nv)

  private[choam] def read: React[Unit, A] =
    React.read(this)

  private[kcas] def unsafeTryRead(): A

  private[kcas] def unsafeTryPerformCas(ov: A, nv: A): Boolean

  private[kcas] def unsafeLazySet(nv: A): Unit

  private[kcas] def unsafeSet(nv: A): Unit

  private[kcas] def dummy(v: Long): Long

  final override def toString: String =
    s"Ref@${Integer.toHexString(this.##)}"

  final override def hashCode: Int =
    System.identityHashCode(this)

  final override def equals(that: Any): Boolean =
    equ(this, that)
}

object Ref {

  private[choam] def mk[A](a: A): Ref[A] =
    new PaddedRefImpl(a)

  /**
   * Only for testing
   *
   * TODO: provide unpadded groups of refs
   * (e.g., Ref2, Ref3) which still have
   * padding at the end.
   */
  private[kcas] def mkUnpadded[A](a: A): Ref[A] =
    new UnpaddedRefImpl(a)
}

private class UnpaddedRefImpl[A](initial: A) extends AtomicReference[A](initial) with Ref[A] {

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

private final class PaddedRefImpl[A](initial: A) extends UnpaddedRefImpl[A](initial) {

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
