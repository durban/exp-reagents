package com.example.rea

final class LockedCounter {

  final class Holder[A](var cnt: A)

  private[this] val h: Holder[Long] =
    new Holder(0L)

  def add(n: Long): Long = this.synchronized {
    val old = h.cnt
    h.cnt = old + n
    old
  }

  def count(): Long = this.synchronized {
    h.cnt
  }
}
