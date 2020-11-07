/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
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

import cats.syntax.all._

// TODO: figure out a way to detect leaked (not retired) instances
trait DebugManaged[T <: IBR.ThreadContext[T, M], M <: IBRManaged[T, M]]
  extends IBRManaged[T, M] { this: M =>

  private[this] var _allocated = 0

  private[this] var _freed = 0

  override protected[kcas] def allocate(tc: T): Unit = {
    val be = this.getBirthEpoch()
    assert(be <= tc.globalContext.epochNumber)
    val re = this.getRetireEpoch()
    assert(re === Long.MaxValue, s"retire epoch is ${re}")
    assert(this._allocated === this._freed)
    this._allocated += 1
  }

  override protected[kcas] def free(tc: T): Unit = {
    val retireEpoch = this.getRetireEpoch()
    assert(retireEpoch <= tc.globalContext.epochNumber)
    assert(this.getBirthEpoch() <= retireEpoch)
    this._freed += 1
    assert(this._allocated === this._freed)
  }

  protected def checkAccess(): Unit = {
    if (this._allocated === this._freed) {
      // currently "free" object, nobody should access it
      throw new AssertionError
    }
  }

  private[kcas] def allocated: Int = _allocated

  private[kcas] def freed: Int = _freed
}
