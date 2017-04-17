package com.example.rea

final class LockedStack[A] {

  private[this] var head: List[A] =
    Nil

  def push(a: A): Unit = this.synchronized {
    head = a :: head
  }

  def tryPop(): Option[A] = this.synchronized {
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