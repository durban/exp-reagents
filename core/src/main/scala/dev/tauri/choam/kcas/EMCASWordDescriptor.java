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

package dev.tauri.choam.kcas;

final class EMCASWordDescriptor<A>
  extends IBRManaged<EMCASTC<A>, EMCASWordDescriptor<A>> {

  private Ref<A> _address;
  private A _ov;
  private A _nv;
  private EMCAS.MCASDescriptor _parent;

  Ref<A> address() {
    return this._address;
  }

  A ov() {
    return this._ov;
  }

  A nv() {
    return this._nv;
  }

  EMCAS.MCASDescriptor parent() {
    return this._parent;
  }

  @SuppressWarnings("unchecked")
  <B> EMCASWordDescriptor<B> cast() {
    return (EMCASWordDescriptor<B>) this;
  }

  EMCASWordDescriptor<A> withParent(EMCASTC<A> tc, EMCAS.MCASDescriptor newParent) {
    EMCASWordDescriptor<A> wd = tc.alloc().cast();
    wd.initializePlain(this.address(), this.ov(), this.nv(), newParent);
    return wd;
  }

  final void initializePlain(Ref<A> address, A ov, A nv, EMCAS.MCASDescriptor parent) {
    this._address = address;
    this._ov = ov;
    this._nv = nv;
    this._parent = parent;
  }

  @Override
  protected final void allocate(EMCASTC<A> tc) {}

  @Override
  protected final void retire(EMCASTC<A> tc) {}

  @Override
  protected final void free(EMCASTC<A> tc) {
    this._address = null;
    this._ov = null;
    this._nv = null;
    this._parent = null;
  }
}
