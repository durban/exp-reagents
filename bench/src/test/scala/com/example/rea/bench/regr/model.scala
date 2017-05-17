package com.example.rea
package bench
package regr

import scala.reflect.ClassTag

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
  primaryMetric: Metric,
) {
  def parameters: Map[String, String] =
    params.getOrElse(Map.empty)

  def repr: String =
    s"${benchmark}(${parameters.map { case (p, v) => s"${p}=${v}" }.mkString(", ")})"
}

@JsonCodec
final case class Metric(
  score: Double,
  scoreError: Double,
) {

  def relativeError: Double =
    (scoreError / score).abs
}
