package com.example.rea
package bench
package regr

import java.io.File
import java.nio.file.{ Paths, Files }
import java.nio.charset.StandardCharsets.UTF_8

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

  final val expBenchMode = "thrpt"
  final val expThreads = 4
  final val maxRelativeError = 0.08
  final val tolerance = 0.04
  final val resultsFile = s"bench${File.separator}results.json"
  final val rrFile = s"bench${File.separator}relative_results.json"

  final val naive = ("kcasName", kcas.KCAS.fqns.NaiveKCAS)
  final val casn = ("kcasName", kcas.KCAS.fqns.CASN)

  val results = load(new File(resultsFile))
  val baseline = results.byClass[BaselineBench]("baseline")

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

  def interestingBenchmarks(results: Results) = Vector(
    results.byClass[CounterBench]("react", casn),
    results.byClass[CounterBench]("react", naive),
    results.byClass[CounterBenchN]("reactN", casn, "n" -> "2"),
    results.byClass[CounterBenchN]("reactN", casn, "n" -> "8"),
    results.byClass[CounterBenchN]("reactN", naive, "n" -> "2"),
    results.byClass[CounterBenchN]("reactN", naive, "n" -> "8"),
    results.byClass[QueueBench]("michaelScottQueue", casn),
    results.byClass[QueueBench]("michaelScottQueue", naive),
    results.byClass[QueueTransferBench]("michaelScottQueue", casn),
    results.byClass[QueueTransferBench]("michaelScottQueue", naive),
    results.byClass[StackBench]("treiberStack", casn),
    results.byClass[StackBench]("treiberStack", naive),
    results.byClass[StackTransferBench]("treiberStack", casn),
    results.byClass[StackTransferBench]("treiberStack", naive),
    results.byClass[CAS1LoopBench]("successfulCAS1Loop", casn),
    results.byClass[CAS1LoopBench]("successfulCAS1Loop", naive),
    results.byClass[FailedCAS1Bench]("failedCAS1", casn),
    results.byClass[FailedCAS1Bench]("failedCAS1", naive),
    results.byClass[KCASLoopBench]("successfulKCASLoop", casn),
    results.byClass[KCASLoopBench]("successfulKCASLoop", naive),
  )

  "Write relative results" in {
    writeRelativeResults(interestingBenchmarks(results), baseline, new File(rrFile))
  }

  "Sanity check" - {

    "Options" in {
      for (r <- results.rss) {
        r.mode should === (expBenchMode)
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

    "Baseline" in {
      val baseline2 = results.byClass[BaselineBench]("baseline2")
      baseline2 should beWithin(1.0, of = baseline)
    }
  }
}
