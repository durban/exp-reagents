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
package util

import org.openjdk.jmh.annotations.{ State, Param, Setup, Scope }

import kcas._

@State(Scope.Thread)
class RandomState {

  private[this] val rnd =
    XorShift()

  def nextInt(): Int =
    rnd.nextInt()

  def nextLong(): Long =
    rnd.nextLong()

  def nextString(): String =
    rnd.nextLong().abs.toString
}

object CommonThreadState {

  /** Approximately 0.3µs (i5-4300M) */
  final val BaseTokens = 128L

  /** BaseTokens << 0 */
  final val LowContention = 0

  final val LowContentionS = "0"

  /** BaseTokens << 4 (≅ 4.8µs) */
  final val HighContention = 4

  final val HighContentionS = "4"
}

@State(Scope.Thread)
class CommonThreadState extends RandomState {

  import CommonThreadState._

  @Param(Array(LowContentionS, HighContentionS))
  private[this] var contention: Int = _

  def tokens: Long =
    BaseTokens << contention

  def halfTokens: Long =
    BaseTokens << (contention - 1)
}

@State(Scope.Thread)
trait KCASImplState {

  @Param(Array(KCAS.fqns.CASN, KCAS.fqns.NaiveKCAS, KCAS.fqns.MCAS, KCAS.fqns.EMCAS))
  private[choam] var kcasName: String = _

  private[choam] implicit var kcasImpl: KCAS = _

  @Setup
  def setupKCASImpl(): Unit = {
    this.kcasImpl = KCAS.unsafeLookup(kcasName)
  }
}

@State(Scope.Thread)
class KCASThreadState extends CommonThreadState with KCASImplState

@State(Scope.Thread)
class KCASImplStateImpl extends KCASImplState
