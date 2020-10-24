/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dev.tauri.choam
package kcas

abstract class KCASSpec extends BaseSpec {

  private final def tryPerformBatch(ops: List[CASD[_]]): Boolean = {
    val desc = ops.foldLeft(kcasImpl.start()) { (d, op) =>
      op match {
        case op: CASD[a] =>
          d.withCAS[a](op.ref, op.ov, op.nv)
      }
    }
    desc.tryPerform()
  }

  "k-CAS" should "succeed if old values match, and there is no contention" in {
    val r1 = Ref.mk("r1")
    val r2 = Ref.mk("r2")
    val r3 = Ref.mk("r3")
    val succ = tryPerformBatch(List(
      CASD(r1, "r1", "x"),
      CASD(r2, "r2", "y"),
      CASD(r3, "r3", "z")
    ))
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
      tryPerformBatch(List(
        CASD(r1, "r1", "x"),
        CASD(r2, "r2", "y"),
        CASD(r3, "r3", "z")
      ))
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

  it should "not accept more than one CAS for the same ref" in {
    val r1 = Ref.mk("r1")
    val r2 = Ref.mk("r2")
    val exc = intercept[Exception] {
      tryPerformBatch(List(
        CASD(r1, "r1", "x"),
        CASD(r2, "r2", "y"),
        CASD(r1, "r1", "x") // this is a duplicate
      ))
    }
    exc.getMessage should include ("Impossible k-CAS")
    r1.unsafeTryRead() shouldBe theSameInstanceAs ("r1")
    r2.unsafeTryRead() shouldBe theSameInstanceAs ("r2")
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
