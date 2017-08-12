package io.sigs.choam
package bench
package util

final class LockedQueue[A](els: Iterable[A]) {

  private[this] val q =
    new java.util.ArrayDeque[A]

  val lock =
    new java.util.concurrent.locks.ReentrantLock

  els.foreach(enqueue)

  def enqueue(a: A): Unit = {
    lock.lock()
    try {
      unlockedEnqueue(a)
    } finally {
      lock.unlock()
    }
  }

  def unlockedEnqueue(a: A): Unit = {
    q.offer(a)
    ()
  }

  def tryDequeue(): Option[A] = {
    lock.lock()
    try {
      unlockedTryDequeue()
    } finally {
      lock.unlock()
    }
  }

  def unlockedTryDequeue(): Option[A] = {
    Option(q.poll())
  }
}
