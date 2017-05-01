package com.example.rea

final class LockedCounter {

  private[this] var cnt =
    0L

  def add(n: Long): Long = this.synchronized {
    val old = cnt
    cnt += n
    old
  }

  def count(): Long = this.synchronized {
    cnt
  }
}
