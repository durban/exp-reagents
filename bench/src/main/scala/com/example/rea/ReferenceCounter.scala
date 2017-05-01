package com.example.rea

import java.util.concurrent.atomic.AtomicReference

final class ReferenceCounter {

  private[this] val ref =
    new AtomicReference[Long](0L)

  def count(): Long =
    ref.get()

  def add(n: Long): Long =
    exec[Long](ref, sum(n))

  private[this] def sum(x: Long)(y: Long): Long =
    x + y

  @tailrec
  private[this] def exec[A](ref: AtomicReference[A], f: A => A): A = {
    val old = ref.get()
    if (ref.compareAndSet(old, f(old))) old
    else exec(ref, f)
  }
}
