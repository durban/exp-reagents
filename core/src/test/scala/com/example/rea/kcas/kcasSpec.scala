package com.example.rea
package kcas

abstract class KCASSpec extends BaseSpec {

  "k-CAS" should "succeed if old values match, and there is no contention" in {
    val r1 = Ref.mk("r1")
    val r2 = Ref.mk("r2")
    val r3 = Ref.mk("r3")
    val succ = kcasImpl.tryPerformBatch(KCASD(List(
      CASD(r1, "r1", "x"),
      CASD(r2, "r2", "y"),
      CASD(r3, "r3", "z")
    )))
    assert(succ)
    r1.unsafeTryRead() shouldBe theSameInstanceAs ("x")
    r2.unsafeTryRead() shouldBe theSameInstanceAs ("y")
    r3.unsafeTryRead() shouldBe theSameInstanceAs ("z")
  }

  it should "fail if any of the old values doesn't match" in {
    val r1 = Ref.mk("r1")
    val r2 = Ref.mk("r2")
    val r3 = Ref.mk("r3")

    def go(): Boolean = {
      kcasImpl.tryPerformBatch(KCASD(List(
        CASD(r1, "r1", "x"),
        CASD(r2, "r2", "y"),
        CASD(r3, "r3", "z")
      )))
    }

    r1.unsafeSet("x")
    assert(!go())
    r1.unsafeTryRead() shouldBe theSameInstanceAs ("x")
    r2.unsafeTryRead() shouldBe theSameInstanceAs ("r2")
    r3.unsafeTryRead() shouldBe theSameInstanceAs ("r3")

    r1.unsafeSet("r1")
    r2.unsafeSet("x")
    assert(!go())
    r1.unsafeTryRead() shouldBe theSameInstanceAs ("r1")
    r2.unsafeTryRead() shouldBe theSameInstanceAs ("x")
    r3.unsafeTryRead() shouldBe theSameInstanceAs ("r3")

    r2.unsafeSet("r2")
    r3.unsafeSet("x")
    assert(!go())
    r1.unsafeTryRead() shouldBe theSameInstanceAs ("r1")
    r2.unsafeTryRead() shouldBe theSameInstanceAs ("r2")
    r3.unsafeTryRead() shouldBe theSameInstanceAs ("x")

    r3.unsafeSet("r3")
    assert(go())
    r1.unsafeTryRead() shouldBe theSameInstanceAs ("x")
    r2.unsafeTryRead() shouldBe theSameInstanceAs ("y")
    r3.unsafeTryRead() shouldBe theSameInstanceAs ("z")
  }
}

class KCASSpecNaiveKCAS
  extends KCASSpec
  with SpecNaiveKCAS

class KCASSpecCASN
  extends KCASSpec
  with SpecCASN

class KCASSpecMCAS
  extends KCASSpec
  with SpecMCAS
