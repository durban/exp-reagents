package io.sigs.choam
package kcas

import java.util.concurrent.atomic.AtomicReference

/** k-CAS-able atomic reference */
final class Ref[A] private (initial: A) {

  private[this] final val value: AtomicReference[A] =
    new AtomicReference(initial)

  def upd[B, C](f: (A, B) => (A, C)): React[B, C] =
    React.upd(this)(f)

  def modify(f: A => A): React[Unit, A] =
    upd[Unit, A] { (a, _) => (f(a), a) }

  val get: React[Unit, A] =
    upd[Unit, A] { (a, _) => (a, a) }

  private[choam] def cas(ov: A, nv: A): React[Unit, Unit] =
    React.cas(this, ov, nv)

  private[choam] def read: React[Unit, A] =
    React.read(this)

  private[kcas] def unsafeTryRead(): A =
    value.get()

  private[kcas] def unsafeTryPerformCas(ov: A, nv: A): Boolean =
    value.compareAndSet(ov, nv)

  private[kcas] def unsafeLazySet(nv: A): Unit =
    value.lazySet(nv)

  private[kcas] def unsafeSet(nv: A): Unit =
    value.set(nv)

  override def toString: String =
    s"Ref@${Integer.toHexString(this.##)}"

  override def hashCode: Int =
    System.identityHashCode(this)

  override def equals(that: Any): Boolean =
    equ(this, that)
}

object Ref {

  def newRef[A](initial: A): React[Unit, Ref[A]] =
    React.newRef(initial)

  private[choam] def mk[A](a: A): Ref[A] =
    new Ref(a)
}
