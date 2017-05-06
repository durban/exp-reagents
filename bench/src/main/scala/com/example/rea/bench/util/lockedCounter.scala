package com.example.rea
package bench
package util

import java.util.concurrent.locks.ReentrantLock

final class LockedCounter {

  final class Holder[A](var cnt: A)

  private[this] val h: Holder[Long] =
    new Holder(0L)

  val lck: ReentrantLock =
    new ReentrantLock

  def add(n: Long): Long = {
    lck.lock()
    try {
      unlockedAdd(n)
    } finally {
      lck.unlock()
    }
  }

  def unlockedAdd(n: Long): Long = {
    val old = h.cnt
    h.cnt = old + n
    old
  }

  def count(): Long = this.synchronized {
    lck.lock()
    try {
      unlockedCount()
    } finally {
      lck.unlock()
    }
  }

  def unlockedCount(): Long =
    h.cnt
}

final class LockedCounterN(n: Int) {

  private[this] val ctrs =
    Array.fill(n)(new LockedCounter)

  def add(n: Long): Unit = {
    ctrs.foreach { _.lck.lock() }
    try {
      ctrs.foreach { _.unlockedAdd(n) }
    } finally {
      ctrs.foreach { _.lck.unlock() }
    }
  }
}
