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

package dev.tauri.choam.kcas;

import java.lang.invoke.VarHandle;
import java.lang.invoke.MethodHandles;
import java.util.concurrent.atomic.AtomicLongFieldUpdater;

/**
 * Base class for objects managed by IBR
 */
public abstract class IBRManaged<T, M extends IBRManaged<T, M>> {

  // TODO: (look into acq/rel modes: http://gee.cs.oswego.edu/dl/html/j9mm.html)?
  // TODO: verify that we really don't need born_before from the paper

  @SuppressWarnings("rawtypes")
  private static final AtomicLongFieldUpdater<IBRManaged> BIRTH_EPOCH =
    AtomicLongFieldUpdater.newUpdater(IBRManaged.class, "_birthEpoch");

  @SuppressWarnings("rawtypes")
  private static final AtomicLongFieldUpdater<IBRManaged> RETIRE_EPOCH =
    AtomicLongFieldUpdater.newUpdater(IBRManaged.class, "_retireEpoch");

  private static final VarHandle BIRTH_EPOCH_VH;

  private static final VarHandle RETIRE_EPOCH_VH;

  static {
    try {
      MethodHandles.Lookup l = MethodHandles.lookup();
      BIRTH_EPOCH_VH = l.findVarHandle(IBRManaged.class, "_birthEpoch", long.class);
      RETIRE_EPOCH_VH = l.findVarHandle(IBRManaged.class, "_retireEpoch", long.class);
    } catch (ReflectiveOperationException ex) {
      throw new ExceptionInInitializerError(ex);
    }
  }

  @SuppressWarnings("unused")
  private volatile long _birthEpoch;

  @SuppressWarnings("unused")
  private volatile long _retireEpoch;

  /** Intrusive linked list (free/retired list) */
  private M _next;

  protected IBRManaged() {
    // TODO: maybe `setOpaque`?
    BIRTH_EPOCH_VH.set(this, Long.MIN_VALUE);
    RETIRE_EPOCH_VH.set(this, Long.MAX_VALUE);
  }

  final M next() { // TODO: -> getNext()
    return this._next;
  }

  final void setNext(M n) {
    this._next = n;
  }

  final long getBirthEpoch() {
    return BIRTH_EPOCH.get(this);
  }

  final void setBirthEpoch(long e) {
    BIRTH_EPOCH.set(this, e);
  }

  final long getRetireEpoch() {
    return RETIRE_EPOCH.get(this);
  }

  final void setRetireEpoch(long e) {
    RETIRE_EPOCH.set(this, e);
  }

  /** Hook for subclasses for performing cleanup */
  abstract protected void free(T tc);

  /** Hook for subclasses for performing initialization */
  abstract protected void allocate(T tc);
}
