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

package io.sigs.choam
package bench
package regr

import java.io.File
import java.nio.file.{ Path, Paths, Files }
import java.nio.charset.StandardCharsets.UTF_8

import scala.reflect.ClassTag

import cats.implicits._
import cats.effect.{ IO, Timer }

import io.circe.syntax._

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.Matcher
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals

import kcas.bench._

class RegressionSpec
  extends AnyFreeSpec
  with Matchers
  with TypeCheckedTripleEquals
  with IOSpec {

  final val expThreads = 4
  final val maxRelativeError = 0.08
  final val tolerance = 0.04
  final val resultsFile = s"bench${File.separator}results.json"
  final val rrFile = s"bench${File.separator}relative_results.json"

  final val naive = ("kcasName", kcas.KCAS.fqns.NaiveKCAS)
  final val casn = ("kcasName", kcas.KCAS.fqns.CASN)
  final val mcas = ("kcasName", kcas.KCAS.fqns.MCAS)

  implicit val timer: Timer[IO] =
    IO.timer(scala.concurrent.ExecutionContext.global)

  val results = load(Paths.get(resultsFile))
  val baseline = results.byClass[BaselineBench]("baseline", "contention" -> "0")

  def load(file: Path): Results = {
    val src = fs2.io.file
      .readAll[IO](file, this.blocker, chunkSize = 1024)
      .through(fs2.text.utf8Decode)
    val results = src.compile.toVector.map { vec =>
      io.circe.parser.decode[Vector[BenchmarkResult]](vec.mkString)
    }
    results.attempt.unsafeRunSync() match {
      case Left(ex) => throw ex
      case Right(Left(err)) => throw err
      case Right(Right(rss)) => Results(rss)
    }
  }

  def writeRelativeResults(rss: Vector[BenchmarkResult], baseline: BenchmarkResult, file: File): Unit = {
    val rrs = rss.map(r => RelativeResult.fromBenchmarkResult(r, baseline))
    val json = rrs.asJson
    val bs = json.spaces2.getBytes(UTF_8)
    Files.write(Paths.get(file.getPath), bs)
    ()
  }

  def beWithin(multiplier: Double, of: BenchmarkResult = baseline): Matcher[BenchmarkResult] = {
    val target = of.primaryMetric.score * multiplier
    val min = target * (1 - tolerance)
    val max = target * (1 + tolerance)
    val m = be >= min and be <= max
    m.compose[BenchmarkResult](_.primaryMetric.score)
  }

  def tagName[A](name: String)(implicit tag: ClassTag[A]): (ClassTag[_], String) =
    (tag, name)

  def interestingBenchmarks(results: Results): Vector[BenchmarkResult] = {
    for {
      contention <- Vector("contention" -> "0", "contention" -> "4")
      kcasImpl <- Vector(naive, casn, mcas)
      method <- Vector(
        tagName[CounterBench]("react"),
        tagName[CounterBenchN]("reactN"),
        tagName[QueueBench]("michaelScottQueue"),
        tagName[QueueTransferBench]("michaelScottQueue"),
        tagName[StackBench]("treiberStack"),
        tagName[StackTransferBench]("treiberStack"),
        tagName[CAS1LoopBench]("successfulCAS1Loop"),
        tagName[FailedCAS1Bench]("failedCAS1")
      )
    } yield results.byClass(method._2, kcasImpl, contention)(method._1)
  }

  "Write relative results" in {
    writeRelativeResults(interestingBenchmarks(results), baseline, new File(rrFile))
  }

  "Sanity check" - {

    "Options" in {
      for (r <- results.rss) {
        r.threads should === (expThreads)
      }
    }

    "Relative errors" ignore {
      for (r <- results.rss) {
        assert(
          r.primaryMetric.relativeError <= maxRelativeError,
          s"-- relative error of ${r.repr}"
        )
      }
    }
  }
}
