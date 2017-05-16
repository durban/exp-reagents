package com.example.rea
package bench
package util

final class XorShift private (private[this] var state: Int) {

  def nextInt(): Int = {
    state ^= (state << 6)
    state ^= (state >>> 21)
    state ^= (state << 7)
    state
  }
}

object XorShift {

  def apply(): XorShift =
    apply(java.util.concurrent.ThreadLocalRandom.current().nextInt())

  def apply(seed: Int): XorShift =
    new XorShift(seed)
}
