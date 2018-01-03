/*
 * Copyright 2017-2018 Daniel Urban and contributors listed in AUTHORS
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

package io.sigs.choam
package async

import cats.implicits._

import Promise._

final class Promise[A] private (ref: kcas.Ref[State[A]]) {

  val tryComplete: React[A, Boolean] = React.computed { a =>
    ref.invisibleRead.flatMap {
      case w @ Waiting(cbs) =>
        ref.cas(w, Done(a)).rmap(_ => true).postCommit(React.lift { _ =>
          cbs.valuesIterator.foreach(_(a))
        })
      case Done(_) =>
        React.ret(false)
    }
  }

  val get: AsyncReact[A] = {
    AsyncReact.lift(ref.invisibleRead).flatMap {
      case Waiting(_) =>
        AsyncReact.delay(new Id).flatMap { id =>
          AsyncReact.kcas.flatMap { implicit kcas =>
            AsyncReact.async[A] { cb =>
              insertCallback(id, cb).unsafePerform(()) match {
                case None =>
                  ()
                case Some(a) =>
                  cb(a)
              }
              (_: Unit) => {
                ref.modify {
                  case Waiting(cbs) =>
                    Waiting(cbs - id)
                  case d @ Done(_) =>
                    d
                }.discard.unsafePerform(())
              }
            }
          }
        }
      case Done(a) =>
        AsyncReact.pure(a)
    }
  }

  private def insertCallback(id: Id, cb: A => Unit): React[Unit, Option[A]] = {
    val kv = (id, cb)
    ref.modify {
      case Waiting(cbs) => Waiting(cbs + kv)
      case d @ Done(_) => d
    }.map {
      case Waiting(_) => None
      case Done(a) => Some(a)
    }
  }
}

object Promise {

  private final class Id

  private sealed abstract class State[A]
  private final case class Waiting[A](cbs: Map[Id, A => Unit]) extends State[A]
  private final case class Done[A](a: A) extends State[A]

  def apply[A]: React[Unit, Promise[A]] =
    React.lift(_ => new Promise[A](kcas.Ref.mk(Waiting(Map.empty))))
}
