/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
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

package io.sigs.choam
package kcas

import scala.collection.JavaConverters._

import org.openjdk.jol.info.ClassLayout

object RefSpec {
  final val fieldName = "value"
  final val targetSize = 160L
}

@deprecated("so that we can test deprecated methods", since = "we need it")
class RefSpec extends BaseSpec {

  import RefSpec._

  // we're not really using this:
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.NaiveKCAS

  def getRightPaddedSize(obj: AnyRef, fieldName: String): Long = {
    val layout = ClassLayout.parseInstance(obj)
    val fields = layout.fields.iterator.asScala.toList
    val valField = fields.filter(_.name === fieldName) match {
      case Nil => fail(s"no '${fieldName}' field found in ${obj} (of class ${obj.getClass})")
      case h :: Nil => h
      case more => fail(s"more than one '${fieldName}' field found: ${more}")
    }

    val start = valField.offset
    val end = layout.instanceSize
    end - start
  }

  "Ref" should "be padded to avoid false sharing" in {
    val ref = Ref.mk("foo")
    val size = getRightPaddedSize(ref, fieldName)
    size should be >= targetSize
    info(s"size is ${size} bytes")
  }


  it should "compute `bigId` correctly" in {
    val ref = new PaddedRefImpl[String]("foo")(
      0xffffffffffffffffL,
      0xaaaaaaaaaaaaaaaaL,
      0xccccccccccccccccL,
      0xdbdbdbdbdbdbdbdbL
    )
    ref.bigId should === (BigInt(
      (
        Vector.fill(8)(0xff.toByte) ++
        Vector.fill(8)(0xaa.toByte) ++
        Vector.fill(8)(0xcc.toByte) ++
        Vector.fill(8)(0xdb.toByte)
      ).toArray
    ))
  }

  "Unpadded Ref" should "not be padded (sanity check)" in {
    val ref = Ref.mkUnpadded("bar")
    val size = getRightPaddedSize(ref, fieldName)
    size should be <= 48L
    info(s"size is ${size} bytes")
  }
}
