/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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

import scala.concurrent.ExecutionContext

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals

class EMCASSpec
  extends AnyFlatSpec
  with Matchers
  with TypeCheckedTripleEquals {

  sealed trait Obj
  final case object A extends Obj
  final case object B extends Obj
  final case object C extends Obj

  implicit val ec: ExecutionContext =
    ExecutionContext.global

  private def polluteTheHeap[A](desc: AnyRef): A =
    desc.asInstanceOf[A]

  "Read" should "help the other operation" in {
    val r1 = Ref.mkWithId("r1")(0L, 0L, 0L, 0L)
    val r2 = Ref.mkWithId("r2")(0L, 0L, 0L, 42L)
    val other: EMCAS.MCASDescriptor = EMCAS
      .start()
      .withCAS(r1, "r1", "x")
      .withCAS(r2, "r2", "y")
      .asInstanceOf[EMCAS.MCASDescriptor]
    other.sort()
    val d0 = other.words.get(0)
    assert(d0.address eq r1)
    r1.unsafeSet(polluteTheHeap[String](d0))
    val res = EMCAS.tryReadOne(r1)
    res should === ("x")
    EMCAS.tryReadOne(r1) should === ("x")
    EMCAS.tryReadOne(r2) should === ("y")
    assert(other.status.get() eq EMCAS.Successful)
  }

  it should "roll back the other op if necessary" in {
    val r1 = Ref.mkWithId("r1")(0L, 0L, 0L, 0L)
    val r2 = Ref.mkWithId("r2")(0L, 0L, 0L, 99L)
    val other = EMCAS
      .start()
      .withCAS(r1, "r1", "x")
      .withCAS(r2, "zzz", "y") // this will fail
      .asInstanceOf[EMCAS.MCASDescriptor]
    other.sort()
    val d0 = other.words.get(0)
    assert(d0.address eq r1)
    r1.unsafeSet(polluteTheHeap[String](d0))
    val res = EMCAS.tryReadOne(r1)
    res should === ("r1")
    EMCAS.tryReadOne(r1) should === ("r1")
    EMCAS.tryReadOne(r2) should === ("r2")
  }
}
