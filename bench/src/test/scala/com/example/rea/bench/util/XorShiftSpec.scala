package com.example.rea
package bench
package util

import org.scalatest.{ FlatSpec, Matchers }
import org.scalactic.TypeCheckedTripleEquals

class XorShiftSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  "XorShift" should "mostly work" in {
    val xs = XorShift()
    val N = 1000000
    val ns = Stream.continually(xs.nextInt()).take(N).toVector
    ns.toSet.size.toDouble should be >= (0.9 * N)
    val negs = ns.filter(n => n < 0).size.toDouble
    negs should be >= (0.4 * N)
    negs should be <= (0.6 * N)
  }
}
