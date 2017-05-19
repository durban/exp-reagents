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

  def writeRelativeResults(rss: Results, baseline: BenchmarkResult, file: File): Unit = {
    val rrs = rss.rss.map(r => RelativeResult.fromBenchmarkResult(r, baseline))
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

  "Sanity check" - {

    "Write relative results" in {
      writeRelativeResults(results, baseline, new File(rrFile))
    }

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

  "Counters" - {

    "CounterBench" in {
      results.byClass[CounterBench]("react", casn) should beWithin(0.05)
      results.byClass[CounterBench]("react", naive) should beWithin(0.09)
    }

    "CounterBenchN" in {
      results.byClass[CounterBenchN]("reactN", casn, "n" -> "2") should beWithin(0.0214)
      results.byClass[CounterBenchN]("reactN", casn, "n" -> "8") should beWithin(0.00315)
      results.byClass[CounterBenchN]("reactN", naive, "n" -> "2") should beWithin(0.0285)
      results.byClass[CounterBenchN]("reactN", naive, "n" -> "8") should beWithin(0.00328)
    }
  }

  "Queues" - {

    "QueueBench" in {
      results.byClass[QueueBench]("michaelScottQueue", casn) should beWithin(0.0142)
      results.byClass[QueueBench]("michaelScottQueue", naive) should beWithin(0.0173)
    }

    "QueueTransferBench" in {
      results.byClass[QueueTransferBench]("michaelScottQueue", casn) should beWithin(0.0138)
      results.byClass[QueueTransferBench]("michaelScottQueue", naive) should beWithin(0.0182)
    }
  }

  "Stacks" - {

    "StackBench" in {
      results.byClass[StackBench]("treiberStack", casn) should beWithin(0.0263)
      results.byClass[StackBench]("treiberStack", naive) should beWithin(0.047)
    }

    "StackTransferBench" in {
      results.byClass[StackTransferBench]("treiberStack", casn) should beWithin(0.0174)
      results.byClass[StackTransferBench]("treiberStack", naive) should beWithin(0.0254)
    }
  }

  "CAS-es" - {

    import kcas.bench._

    "CAS1LoopBench" in {
      results.byClass[CAS1LoopBench]("successfulCAS1Loop", casn) should beWithin(0.0431)
      results.byClass[CAS1LoopBench]("successfulCAS1Loop", naive) should beWithin(0.0666)
    }

    "FailedCAS1Bench" in {
      results.byClass[FailedCAS1Bench]("failedCAS1", casn) should beWithin(0.219)
      results.byClass[FailedCAS1Bench]("failedCAS1", naive) should beWithin(0.294)
    }

    "KCASLoopBench" in {
      results.byClass[KCASLoopBench]("successfulKCASLoop", casn) should beWithin(0.00928)
      results.byClass[KCASLoopBench]("successfulKCASLoop", naive) should beWithin(0.0106)
    }
  }
}
