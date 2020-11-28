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

package dev.tauri

package object choam {

  private[choam] type tailrec = scala.annotation.tailrec

  // TODO: Using these always leaves a check for
  // TODO: the package object in the bytecode.
  // TODO: Maybe put these in a Java class as static
  // TODO: methods...

  @inline
  private[choam] def box[A](a: A): AnyRef =
    a.asInstanceOf[AnyRef]

  @inline
  private[choam] def equ[A](x: A, y: A): Boolean =
    box(x) eq box(y)

  @inline
  private[choam] def isNull[A](a: A): Boolean =
    box(a) eq null

  @inline
  private[choam] def nullOf[A]: A =
    null.asInstanceOf[A]

  private[choam] def impossible(s: String): Nothing =
    throw new IllegalStateException(s)
}
