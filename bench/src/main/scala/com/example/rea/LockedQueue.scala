package com.example.rea

final class LockedQueue[A] {

  private[this] val q =
    new scala.collection.mutable.Queue[A]

  def enqueue(a: A): Unit = this.synchronized {
    q.enqueue(a)
  }

  def tryDequeue(): Option[A] = this.synchronized {
    if (q.isEmpty) None
    else Some(q.dequeue())
  }
}
