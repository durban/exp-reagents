package com.example.rea
package bench
package regr

import java.io.File
import java.nio.file.{ Paths, Files }
import java.nio.charset.StandardCharsets.UTF_8

import scala.reflect.ClassTag

import cats.implicits._

import io.iteratee._
import io.iteratee.files.either

import io.circe.syntax._
import io.circe.streaming.{ byteParser, decoder }

import org.scalatest.{ FreeSpec, Matchers }
import org.scalatest.matchers.Matcher
import org.scalactic.TypeCheckedTripleEquals

import kcas.bench._

class RegressionSpec extends FreeSpec with Matchers with TypeCheckedTripleEquals {

  final val expThreads = 4
  final val maxRelativeError = 0.08
  final val tolerance = 0.04
  final val resultsFile = s"bench${File.separator}results.json"
  final val rrFile = s"bench${File.separator}relative_results.json"

  final val naive = ("kcasName", kcas.KCAS.fqns.NaiveKCAS)
  final val casn = ("kcasName", kcas.KCAS.fqns.CASN)
  final val mcas = ("kcasName", kcas.KCAS.fqns.MCAS)

  val results = load(new File(resultsFile))
  val baseline = results.byClass[BaselineBench]("baseline", "shift" -> "0")

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

  def writeRelativeResults(rss: Vector[BenchmarkResult], baseline: BenchmarkResult, file: File): Unit = {
    val rrs = rss.map(r => RelativeResult.fromBenchmarkResult(r, baseline))
    val json = rrs.asJson
    val bs = json.spaces2.getBytes(UTF_8)
    Files.write(Paths.get(file.getPath), bs)
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
      contention <- Vector("shift" -> "0", "shift" -> "4")
      kcasImpl <- Vector(naive, casn, mcas)
      method <- Vector(
        tagName[CounterBench]("react"),
        tagName[CounterBenchN]("reactN"),
        tagName[QueueBench]("michaelScottQueue"),
        tagName[QueueTransferBench]("michaelScottQueue"),
        tagName[StackBench]("treiberStack"),
        tagName[StackTransferBench]("treiberStack"),
        tagName[CAS1LoopBench]("successfulCAS1Loop"),
        tagName[FailedCAS1Bench]("failedCAS1"),
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

    "Relative errors" in {
      for (r <- results.rss) {
        assert(
          r.primaryMetric.relativeError <= maxRelativeError,
          s"-- relative error of ${r.repr}"
        )
      }
    }
  }
}
