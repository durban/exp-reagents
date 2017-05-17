package com.example.rea
package bench
package util

import org.scalatest.{ FlatSpecLike, Matchers }
import org.scalactic.TypeCheckedTripleEquals

import fs2.Strategy

trait BaseSpec extends FlatSpecLike with Matchers with TypeCheckedTripleEquals {

  implicit val str: Strategy =
    Strategy.fromExecutionContext(scala.concurrent.ExecutionContext.global)
}
