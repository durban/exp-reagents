package com.example.rea
package bench
package util

final class LockedStack[A] {

  private[this] var head: List[A] =
    Nil

  val lock =
    new java.util.concurrent.locks.ReentrantLock

  def push(a: A): Unit = {
    lock.lock()
    try {
      unlockedPush(a)
    } finally {
      lock.unlock()
    }
  }

  def unlockedPush(a: A): Unit = {
    head = a :: head
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
      case h :: t =>
        head = t
        Some(h)
      case Nil =>
        None
    }
  }

  def length: Int = this.synchronized {
    head.length
  }
}
