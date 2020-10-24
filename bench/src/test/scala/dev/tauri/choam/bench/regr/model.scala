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

package dev.tauri.choam
package bench
package regr

import java.math.{ MathContext, RoundingMode }

import scala.reflect.ClassTag
import scala.math.BigDecimal

import cats.implicits._

import io.circe.generic.JsonCodec

final case class Results(
  rss: Vector[BenchmarkResult]
) {

  def byClass[A](method: String, params: (String, String)*)(implicit ct: ClassTag[A]): BenchmarkResult =
    lookup(ct.runtimeClass.getName + "." + method, params: _*)

  def lookup(name: String, params: (String, String)*): BenchmarkResult = {
    val parameters = Map(params: _*)
    rss.filter(_.benchmark === name).find { r =>
      r.parameters === parameters
    }.getOrElse {
      throw new IllegalArgumentException(s"no such benchmark: '${name}' (${params})")
    }
  }
}

@JsonCodec
final case class BenchmarkResult(
  benchmark: String,
  mode: String,
  threads: Int,
  params: Option[Map[String, String]],
  primaryMetric: Metric
) {
  def parameters: Map[String, String] =
    params.getOrElse(Map.empty)

  def repr: String =
    s"${benchmark}(${parameters.map { case (p, v) => s"${p}=${v}" }.mkString(", ")})"
}

@JsonCodec
final case class Metric(
  score: BigDecimal,
  scoreError: BigDecimal
) {

  def relativeError: BigDecimal =
    (scoreError / score).abs
}

@JsonCodec
final case class RelativeResult(
  benchmark: String,
  relativeScore: BigDecimal
)

object RelativeResult {

  def fromBenchmarkResult(r: BenchmarkResult, baseline: BenchmarkResult): RelativeResult = {
    val blScore = baseline.primaryMetric.score
    val score = r.primaryMetric.score
    val err = r.primaryMetric.scoreError
    val relScore = score / blScore
    val maxRelScore = (score + err) / blScore
    val minRelScore = (score - err) / blScore
    val roundedRelScore = round(relScore, minRelScore, maxRelScore)
    RelativeResult(r.repr, roundedRelScore)
  }

  private val whole = new MathContext(0, RoundingMode.HALF_EVEN)

  private def round(score: BigDecimal, min: BigDecimal, max: BigDecimal): BigDecimal = {
    val mc = roundCommon(min, max)
    score.apply(mc)
  }

  private def roundCommon(a: BigDecimal, b: BigDecimal): MathContext = {
    // TODO: clean this up
    val as = a.bigDecimal.toPlainString
    val bs = b.bigDecimal.toPlainString
    if (as.contains('.') && bs.contains('.')) {
      val af = as.split('.')(1)
      val bf = bs.split('.')(1)
      val prec = (af zip bf).takeWhile { case (a, b) => a === b }.length
      new MathContext(prec + 1, RoundingMode.HALF_EVEN)
    } else {
      whole
    }
  }
}
