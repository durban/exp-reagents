package io.sigs.choam
package bench
package util

import scala.concurrent.ExecutionContext

import org.scalatest.{ FlatSpecLike, Matchers }
import org.scalactic.TypeCheckedTripleEquals


trait BaseSpec extends FlatSpecLike with Matchers with TypeCheckedTripleEquals {

  implicit val ec: ExecutionContext =
    ExecutionContext.global
}
