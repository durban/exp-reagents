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

addSbtPlugin("io.get-coursier" % "sbt-coursier" % "1.0.0-RC3")
addSbtPlugin("org.tpolecat" % "tut-plugin" % "0.5.2")
addSbtPlugin("org.typelevel" % "sbt-typelevel" % "0.3.2")
addSbtPlugin("org.scalastyle" % "scalastyle-sbt-plugin" % "0.8.0")
addSbtPlugin("pl.project13.scala" % "sbt-jmh" % "0.2.25")
addSbtPlugin("pl.project13.sbt" % "sbt-jcstress" % "0.1.0")
addSbtPlugin("com.geirsson" % "sbt-scalafmt" % "0.6.8")

resolvers += Resolver.url(
  "sbt-jcstress-repo",
  url("http://dl.bintray.com/ktosopl/sbt-plugins")
)(Resolver.ivyStylePatterns)
