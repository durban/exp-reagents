package io.sigs.choam
package bench
package util

class ReferenceTreiberStackSpec extends BaseSpec {

  "length" should "be correct" in {
    new ReferenceTreiberStack[Int].length should === (0)
    new ReferenceTreiberStack[Int](Nil).length should === (0)
    new ReferenceTreiberStack[Int](1 :: 2 :: 3 :: Nil).length should === (3)
  }
}
