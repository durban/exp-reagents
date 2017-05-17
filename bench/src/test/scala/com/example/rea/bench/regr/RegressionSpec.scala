package com.example.rea
package bench
package regr

import java.io.File

import cats.implicits._

import io.iteratee._
import io.iteratee.files.either

import io.circe.streaming.{ byteParser, decoder }

class RegressionSpec extends util.BaseSpec {

  final val resultsFile = s"bench${File.separator}results.json"
  final val expBenchMode = "thrpt"
  final val expThreads = 4
  final val maxRelativeError = 0.1

  val results = load(new File(resultsFile))

  def load(file: File): Results = {
    val src = either.readBytes(file)
    val results = src
      .through(byteParser)
      .through(decoder[Either[Throwable, ?], BenchmarkResult])
    results.into(Iteratee.consume) match {
      case Left(ex) => throw ex
      case Right(rss) => Results(rss)
    }
  }

  "Sanity check" should "pass" in {
    for (r <- results.rss) {
      r.mode should === (expBenchMode)
      r.threads should === (expThreads)
    }
  }

  "Errors" should "be relatively low" in {
    for (r <- results.rss) {
      assert(
        r.primaryMetric.relativeError <= maxRelativeError,
        s"-- relative error of ${r.repr}"
      )
    }
  }
}
