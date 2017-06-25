package io.sigs.choam
package bench
package util

final class LockedStack[A](els: Iterable[A]) {

  def this() =
    this(Iterable.empty)

  private[this] var head: TsList[A] =
    TsList.End

  val lock =
    new java.util.concurrent.locks.ReentrantLock

  lock.lock()
  try {
    els.foreach(unlockedPush)
  } finally {
    lock.unlock()
  }

  def push(a: A): Unit = {
    lock.lock()
    try {
      unlockedPush(a)
    } finally {
      lock.unlock()
    }
  }

  def unlockedPush(a: A): Unit = {
    head = TsList.Cons(a, head)
  }

  def tryPop(): Option[A] = {
    lock.lock()
    try {
      unlockedTryPop()
    } finally {
      lock.unlock()
    }
  }

  def unlockedTryPop(): Option[A] = {
    head match {
      case TsList.Cons(h, t) =>
        head = t
        Some(h)
      case TsList.End =>
        None
    }
  }

  def length: Int = this.synchronized {
    head.length
  }
}
