/*
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2016-2020 Daniel Urban and contributors listed in NOTICE.txt
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
package kcas

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.annotations.Outcome.Outcomes
import org.openjdk.jcstress.annotations.Expect._
import org.openjdk.jcstress.infra.results.LL_Result

@JCStressTest
@State
@Description("IBRStackFast pop/push should be atomic")
@Outcomes(Array(
  new Outcome(id = Array("z, List(x, y)", "z, List(y, x)"), expect = ACCEPTABLE, desc = "Pop is the first"),
  new Outcome(id = Array("x, List(y, z)", "y, List(x, z)"), expect = ACCEPTABLE, desc = "Pop one of the pushed values")
))
class IBRStackFastTest {

  private[this] val stack =
    IBRStackFast[String]("z")

  @Actor
  def push1(): Unit = {
    this.stack.push("x", IBRStackFast.threadLocalContext())
  }

  @Actor
  def push2(): Unit = {
    this.stack.push("y", IBRStackFast.threadLocalContext())
  }

  @Actor
  def pop(r: LL_Result): Unit = {
    r.r1 = this.stack.tryPop(IBRStackFast.threadLocalContext())
  }

  @Arbiter
  def arbiter(r: LL_Result): Unit = {
    r.r2 = stack.unsafeToList(IBRStackFast.threadLocalContext())
  }
}
