package io.sigs.choam
package kcas

import scala.collection.JavaConverters._

import org.openjdk.jol.info.ClassLayout

object RefSpec {
  final val fieldName = "value"
  final val targetSize = 128L
}

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

  "Unpadded Ref" should "not be padded (sanity check)" in {
    val ref = Ref.mkUnpadded("bar")
    val size = getRightPaddedSize(ref, fieldName)
    size should be <= 16L
    info(s"size is ${size} bytes")
  }
}
