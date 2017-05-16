package com.example.rea
package bench
package util

import scala.collection.JavaConverters._

object Prefill {

  def prefill(): Iterable[String] =
    Iterable.fill(10000)(scala.util.Random.nextString(16))

  def forJava(): java.util.Collection[String] =
    prefill().toList.asJava
}
