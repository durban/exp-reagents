/*
 * Copyright 2016-2017 Daniel Urban
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

scalaVersion in ThisBuild := "2.12.2-bin-typelevel-4"
crossScalaVersions in ThisBuild := Seq((scalaVersion in ThisBuild).value, "2.11.11-bin-typelevel-4")
scalaOrganization in ThisBuild := "org.typelevel"

lazy val core = project.in(file("core"))
  .settings(name := "choam-core")
  .settings(commonSettings)

lazy val bench = project.in(file("bench"))
  .settings(name := "choam-bench")
  .settings(commonSettings)
  .settings(
    libraryDependencies ++= (
      dependencies.fs2
      ++ Seq(dependencies.scalaStm)
      ++ dependencies.circe.map(_ % Test)
      ++ dependencies.iteratee.map(_ % Test)
    )
  )
  .settings(macroSettings)
  .enablePlugins(JmhPlugin)
  .dependsOn(core)

lazy val stress = project.in(file("stress"))
  .settings(name := "choam-stress")
  .settings(commonSettings)
  .settings(macroSettings)
  .enablePlugins(JCStressPlugin)
  .dependsOn(core)

lazy val commonSettings = Seq[Setting[_]](
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-language:higherKinds,experimental.macros",
    "-opt:l:project",
    "-Xlint:_",
    "-Xfuture",
    "-Xfatal-warnings",
    "-Xstrict-patmat-analysis",
    "-Xelide-below", "INFO",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Ypartial-unification",
    "-Ywarn-unused:implicits",
    "-Ywarn-unused:imports",
    "-Ywarn-unused:locals",
    "-Ywarn-unused:patvars",
    "-Ywarn-unused:params",
    "-Ywarn-unused:privates"
  ),
  scalacOptions := scalacOptions.value.flatMap {
    case opt @ "-Ywarn-unused:_" =>
      if (scalaVersion.value.startsWith("2.12")) opt :: Nil
      else Nil
    case opt =>
      opt :: Nil
  },
  scalacOptions in (Compile, console) ~= { _.filterNot("-Ywarn-unused:imports" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),
  parallelExecution in Test := false,

  libraryDependencies ++= Seq(
    Seq(
      dependencies.cats,
      dependencies.catsFree,
      dependencies.catsEffect,
      dependencies.shapeless
    ),
    dependencies.fs2.map(_ % "test-internal"),
    dependencies.test.map(_ % "test-internal")
  ).flatten,
  organization := "io.sigs",
  publishMavenStyle := true,
  publishArtifact := false, // TODO,
  licenses := Seq("Apache 2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0.txt"))
) ++ typelevelDefaultSettings

lazy val macroSettings = Seq(
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)
)

lazy val dependencies = new {

  val catsVersion = "0.9.0"
  val circeVersion = "0.8.0"
  val iterateeVersion = "0.11.0"
  val fs2Version = "0.10.0-M2"

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.2"
  val cats = "org.typelevel" %% "cats-core" % catsVersion
  val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  val catsEffect = "org.typelevel" %% "cats-effect" % "0.4-c257223"

  val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "io.circe" %% "circe-streaming" % circeVersion
  )

  val iteratee = Seq(
    "io.iteratee" %% "iteratee-core" % iterateeVersion,
    "io.iteratee" %% "iteratee-files" % iterateeVersion
  )

  val scodec = Seq(
    "org.scodec" %% "scodec-bits" % "1.1.2",
    "org.scodec" %% "scodec-core" % "1.10.3",
    "org.scodec" %% "scodec-stream" % "1.0.1",
    "org.scodec" %% "scodec-cats" % "0.3.0"
  )

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version
  )

  val laws = Seq(
    "org.typelevel" %% "cats-laws" % catsVersion,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.4"
  )

  val test = Seq(
    "org.scalatest" %% "scalatest" % "3.0.2",
    "org.typelevel" %% "cats-laws" % catsVersion
  )

  val scalaz = "org.scalaz" %% "scalaz-effect" % "7.2.8"

  val scalaStm = "org.scala-stm" %% "scala-stm" % "0.8"
}

addCommandAlias("measurePerformance", "bench/jmh:run -t max -foe true -rf json -rff results.json .*")
addCommandAlias("measureKCAS", "bench/jmh:run -t max -foe true -rf json -rff results_kcas.json .*ResourceAllocation")
