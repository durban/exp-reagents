package com.example.rea

final class LockedQueue[A] {

  private[this] val q =
    new java.util.ArrayDeque[A]

  def enqueue(a: A): Unit = this.synchronized {
    q.offer(a)
  }

  def tryDequeue(): Option[A] = this.synchronized {
    Option(q.poll())
  }
}
