package com.example.rea
package kcas

import java.util.concurrent.atomic.AtomicReference

/** k-CAS-able atomic reference */
final class Ref[A] private (initial: A) {

  private[this] final val value: AtomicReference[A] =
    new AtomicReference(initial)

  def upd[B, C](f: (A, B) => (A, C)): React[B, C] =
    React.upd(this)(f)

  private[rea] def cas(ov: A, nv: A): React[Unit, Unit] =
    React.cas(this, ov, nv)

  private[rea] def read: React[Unit, A] =
    React.read(this)

  private[kcas] def unsafeTryRead(): A =
    value.get()

  private[kcas] def unsafeTryPerformCas(ov: A, nv: A): Boolean =
    value.compareAndSet(ov, nv)

  private[kcas] def unsafeLazySet(nv: A): Unit =
    value.lazySet(nv)

  override def toString: String =
    s"Ref@${Integer.toHexString(this.##)}"
}

object Ref {

  def unapply[A](ref: Ref[A])(implicit kcas: KCAS): Some[A] =
    Some(ref.read.run)

  private[rea] def mk[A](a: A): Ref[A] =
    new Ref(a)
}
