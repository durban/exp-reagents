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

lazy val core = project.in(file("core"))
  .settings(name := "choam-core")
  .settings(commonSettings)
  .settings(tutSettings)

lazy val bench = project.in(file("bench"))
  .settings(name := "choam-bench")
  .settings(commonSettings)
  .enablePlugins(JmhPlugin)
  .dependsOn(core)

lazy val commonSettings = Seq[Setting[_]](
  scalaVersion := "2.12.1",
  crossScalaVersions := Seq(scalaVersion.value, "2.11.8"),
  scalaOrganization := "org.typelevel",
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
    "-Yno-adapted-args",
    "-Ywarn-numeric-widen",
    "-Ywarn-dead-code",
    "-Ypartial-unification",
    "-Ywarn-unused-import"
  ),
  scalacOptions := scalacOptions.value.flatMap {
    case opt @ "-Xstrict-patmat-analysis" =>
      if (scalaVersion.value.startsWith("2.12")) opt :: Nil
      else Nil
    case opt =>
      opt :: Nil
  },
  scalacOptions in (Compile, console) ~= { _.filterNot("-Ywarn-unused-import" == _) },
  scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
  addCompilerPlugin("org.spire-math" % "kind-projector" % "0.9.3" cross CrossVersion.binary),

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

lazy val dependencies = new {

  val catsVersion = "0.9.0"
  val circeVersion = "0.7.1"

  val shapeless = "com.chuusai" %% "shapeless" % "2.3.2"
  val cats = "org.typelevel" %% "cats-core" % catsVersion
  val catsFree = "org.typelevel" %% "cats-free" % catsVersion
  val catsEffect = "org.typelevel" %% "cats-effect" % "0.1-0848c9b"

  val circe = Seq(
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion
  )

  val scodec = Seq(
    "org.scodec" %% "scodec-bits" % "1.1.2",
    "org.scodec" %% "scodec-core" % "1.10.3",
    "org.scodec" %% "scodec-stream" % "1.0.1",
    "org.scodec" %% "scodec-cats" % "0.3.0"
  )

  val fs2 = Seq(
    "co.fs2" %% "fs2-core" % "0.9.5",
    "co.fs2" %% "fs2-io" % "0.9.5",
    "co.fs2" %% "fs2-cats" % "0.3.0"
  )

  val laws = Seq(
    "org.typelevel" %% "cats-laws" % catsVersion,
    "com.github.alexarchambault" %% "scalacheck-shapeless_1.13" % "1.1.4"
  )

  val test = Seq(
    "org.scalatest" %% "scalatest" % "3.0.1",
    "org.typelevel" %% "cats-laws" % catsVersion
  )

  val scalaz = "org.scalaz" %% "scalaz-effect" % "7.2.8"
}
