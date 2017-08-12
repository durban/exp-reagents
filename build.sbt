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

scalaVersion in ThisBuild := "2.12.3-bin-typelevel-4"
// TODO: this doesn't work (no 2.11):
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
      dependencies.scalaStm
      +: (dependencies.circe.map(_ % Test)
      ++ dependencies.iteratee.map(_ % Test))
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

lazy val layout = project.in(file("layout"))
  .settings(name := "choam-layout")
  .settings(commonSettings)
  .settings(
    libraryDependencies += dependencies.jol % Test,
    fork in Test := true // JOL doesn't like sbt classpath
  )
  .dependsOn(core % "compile->compile;test->test")

lazy val commonSettings = Seq[Setting[_]](
  scalacOptions ++= Seq(
    "-feature",
    "-deprecation",
    "-unchecked",
    "-encoding", "UTF-8",
    "-language:higherKinds,experimental.macros",
    "-Xlint:_",
    "-Xfuture",
    "-Xfatal-warnings",
    "-Xstrict-patmat-analysis",
    "-Xelide-below", "INFO",
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Ywarn-value-discard",
    "-Ypartial-unification"
  ),
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.11")) {
      Seq(
        "-Ywarn-unused",
        "-Ywarn-unused-import"
      )
    } else {
      Seq(
        "-Ywarn-unused:implicits",
        "-Ywarn-unused:imports",
        "-Ywarn-unused:locals",
        "-Ywarn-unused:patvars",
        "-Ywarn-unused:params",
        "-Ywarn-unused:privates"
      )
    }
  },
  scalacOptions ++= {
    if (scalaVersion.value.startsWith("2.11")) {
      Seq(
        "-Yopt:l:project"
      )
    } else {
      Seq(
        "-opt:l:inline",
        "-opt-inline-from:<sources>"
      )
    }
  },
  scalacOptions in (Compile, console) ~= { _.filterNot("-Ywarn-unused:imports" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.4" cross CrossVersion.binary),
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
)

lazy val macroSettings = Seq(
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.patch)
)

lazy val dependencies = new {

  val catsVersion = "1.0.0-MF"
  val circeVersion = "0.8.0"
  val iterateeVersion = "0.12.0"
  val fs2Version = "0.10.0-M5"

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.2"
  val cats = "org.typelevel" %% "cats-core" % catsVersion
  val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  val catsEffect = "org.typelevel" %% "cats-effect" % "0.4"

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

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % fs2Version,
    "co.fs2" %% "fs2-io" % fs2Version
  )

  val test = Seq(
    "org.scalatest" %% "scalatest" % "3.0.3",
    "org.typelevel" %% "cats-laws" % catsVersion
  )

  val scalaStm = "org.scala-stm" %% "scala-stm" % "0.8"

  val jol = "org.openjdk.jol" % "jol-core" % "0.8"
}

addCommandAlias("measurePerformance", "bench/jmh:run -t max -foe true -rf json -rff results.json .*")
addCommandAlias("measureFS", "bench/jmh:run -t max -foe true -rf json -rff results_fs.json .*FalseSharing")
addCommandAlias("measureKCAS", "bench/jmh:run -t max -foe true -rf json -rff results_kcas.json .*ResourceAllocationKCAS")
addCommandAlias("measureReact", "bench/jmh:run -t max -foe true -rf json -rff results_react.json .*ResourceAllocationReact")
addCommandAlias("measureCombinators", "bench/jmh:run -t max -foe true -rf json -rff results_combinators.json .*CombinatorBench")
addCommandAlias("profileReact", "bench/jmh:run -t max -foe true -prof stack:lines=3 -rf text -rff profile_react.txt .*ResourceAllocationReact")

