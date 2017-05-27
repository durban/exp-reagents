package com.example.rea

abstract class TreiberStackSpec extends BaseSpec {

  "TreiberStack" should "include the elements passed to its constructor" in {
    new TreiberStack[Int]().unsafeToList should === (Nil)
    new TreiberStack[Int](1 :: 2 :: 3 :: Nil).unsafeToList should === (3 :: 2 :: 1 :: Nil)
  }
}

class TreiberStackSpecNaiveKCAS
  extends TreiberStackSpec
  with SpecNaiveKCAS

class TreiberStackSpecCASN
  extends TreiberStackSpec
  with SpecCASN

class TreiberStackSpecMCAS
  extends TreiberStackSpec
  with SpecMCAS
