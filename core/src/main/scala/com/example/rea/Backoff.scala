package com.example.rea

import java.util.concurrent.ThreadLocalRandom

// TODO: make thread-local instances, reset
// TODO: and reuse them as needed (to avoid allocations)

/**
 * Truncated exponential backoff.
 *
 * This class is not thread-safe.
 */
private[rea] final class Backoff {

  private[this] var c: Int =
    0

  private[this] var seed: Int = {
    def go(): Int = {
      val r: Int = ThreadLocalRandom.current().nextInt()
      if (r == 0) go()
      else r
    }
    go()
  }

  def backoff(): Unit = {
    val max: Int = {
      if (c < Backoff.max) c += 1
      1 << (c + Backoff.shift)
    }
    val wait: Int = {
      val w: Int = seed % max
      if (w < 0) -w
      else w
    }
    backoffFix(wait)
  }

  def backoffFix(amount: Int): Unit = {
    this.seed = spin(amount, this.seed)
  }

  private def spin(n: Int, s: Int): Int = {
    var cnt: Int = n
    var seed: Int = s
    while ((cnt > 0) || (seed == 1)) {
      seed = xorShift(seed)
      cnt -= 1
    }
    seed
  }

  private def xorShift(s: Int): Int = {
    var seed = s
    seed ^= seed << 1
    seed ^= seed >>> 3
    seed ^= seed << 10
    seed
  }
}

private final object Backoff {
  private final val max: Int =
    Runtime.getRuntime().availableProcessors() + 1 // FIXME
  private final val shift =
    6 // TODO: determine the correct value
}
