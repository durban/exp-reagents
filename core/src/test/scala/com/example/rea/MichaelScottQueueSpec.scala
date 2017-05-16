package com.example.rea

abstract class MichaelScottQueueSpec extends BaseSpec {

  "MichaelScottQueue" should "include the elements passed to its constructor" in {
    new MichaelScottQueue[Int]().unsafeToList should === (Nil)
    new MichaelScottQueue[Int](1 :: 2 :: 3 :: Nil).unsafeToList should === (1 :: 2 :: 3 :: Nil)
  }
}

class MichaelScottQueueSpecNaiveKCAS
  extends MichaelScottQueueSpec
  with SpecNaiveKCAS

class MichaelScottQueueSpecCASN
  extends MichaelScottQueueSpec
  with SpecCASN
