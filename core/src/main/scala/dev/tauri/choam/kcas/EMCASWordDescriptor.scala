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

import java.lang.invoke.VarHandle

final class EMCASWordDescriptor[A]
  extends IBRDebugManaged[EMCASTC[A], EMCASWordDescriptor[A]] {

  private[this] var _address: Ref[A] = _
  private[this] var _ov: A = _
  private[this] var _nv: A = _
  private[this] var _parent: EMCAS.MCASDescriptor = _

  def address = this._address
  def ov = this._ov
  def nv = this._nv
  def parent = this._parent

  def cast[B]: EMCASWordDescriptor[B] =
    this.asInstanceOf[EMCASWordDescriptor[B]]

  def withParent(tc: EMCASTC[A], newParent: EMCAS.MCASDescriptor): EMCASWordDescriptor[A] = {
    val wd = tc.alloc().cast[A]
    wd.initialize(this.address, this.ov, this.nv, newParent)
    wd
  }

  final def initialize(address: Ref[A], ov: A, nv: A, parent: EMCAS.MCASDescriptor): Unit = {
    this._address = address
    this._ov = ov
    this._nv = nv
    this._parent = parent
    VarHandle.releaseFence()
  }

  final def reset(): Unit = {
    this._address = null
    this._ov = nullOf[A]
    this._nv = nullOf[A]
    this._parent = null
    VarHandle.releaseFence()
  }

  protected[kcas] final override def allocate(tc: EMCASTC[A]) =
    super.allocate(tc)

  protected[kcas] final override def retire(tc: EMCASTC[A]) = {
    val finalValue = if (this.parent.getStatusPlain() eq EMCASStatus.SUCCESSFUL) {
      this.nv
    } else { // FAILED (since it is not ACTIVE for sure)
      this.ov
    }
    this.address.unsafeTryPerformCas(this.asInstanceOf[A], finalValue)
    this.reset()
    super.retire(tc)
  }

  protected[kcas] final override def free(tc: EMCASTC[A]) =
    super.free(tc)
}
