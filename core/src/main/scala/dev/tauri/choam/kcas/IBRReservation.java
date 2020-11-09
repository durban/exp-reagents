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

/** The epoch interval reserved by a thread */
final class IBRReservation {

  private static final VarHandle LOWER;
  private static final VarHandle UPPER;

  static {
    try {
      MethodHandles.Lookup l = MethodHandles.lookup();
      LOWER = l.findVarHandle(IBRReservation.class, "_lower", long.class);
      UPPER = l.findVarHandle(IBRReservation.class, "_upper", long.class);
    } catch (ReflectiveOperationException ex) {
      throw new ExceptionInInitializerError(ex);
    }
  }

  @SuppressWarnings("unused")
  private long _lower;

  @SuppressWarnings("unused")
  private long _upper;

  IBRReservation(long initial) {
    this.setLower(initial);
    this.setUpper(initial);
  }

  // Only the owner thread writes lower/upper,
  // other threads just read it. So the owner
  // writes them with `setRelease`, and others
  // read the with `getAcquire`.

  final long getLower() {
    return (long) LOWER.getAcquire(this);
  }

  final void setLower(long value) {
    LOWER.setRelease(this, value);
  }

  final long getUpper() {
    return (long) UPPER.getAcquire(this);
  }

  final void setUpper(long value) {
    UPPER.setRelease(this, value);
  }
}
