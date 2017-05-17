package com.example.rea
package bench
package util

class XorShiftSpec extends BaseSpec {

  "XorShift" should "mostly work" in {
    val xs = XorShift()
    val N = 1000000

    // check Ints:
    val ns = Stream.continually(xs.nextInt()).take(N).toVector
    ns.toSet.size.toDouble should be >= (0.9 * N)
    val negs = ns.filter(n => n < 0).size.toDouble
    negs should be >= (0.4 * N)
    negs should be <= (0.6 * N)

    // check Longs:
    val ms = Stream.continually(xs.nextLong()).take(N).toVector
    ms.toSet.size.toDouble should be >= (0.9 * N)
    val negls = ms.filter(m => m < 0).size.toDouble
    negls should be >= (0.4 * N)
    negls should be <= (0.6 * N)
  }
}
