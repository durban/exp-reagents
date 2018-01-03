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

import cats.{ ~>, Monad }
import cats.free._
import cats.data.{ Kleisli, OptionT }
import cats.effect._

import kcas.{ Ref, KCAS }

object AsyncReact {

  type Cancel = Unit => Unit

  private[async] final class Id

  type _Base
  trait _Tag extends Any
  type Type[A] <: _Base with _Tag

  private type Impl[A] = Free[ArOp, A]

  private def wrap[A](impl: Impl[A]): Type[A] =
    impl.asInstanceOf[Type[A]]

  private def unwrap[A](wrapped: Type[A]): Impl[A] =
    wrapped.asInstanceOf[Impl[A]]

  def pure[A](a: A): Type[A] =
    wrap(Free.pure(a))

  def delay[A](a: => A): Type[A] =
    wrap(Free.liftF(Delay(() => a)))

  def async[A](r: (A => Unit) => Cancel): Type[A] =
    wrap(Free.liftF(RealAsync(r)))

  def lift[A](r: React[Unit, A]): Type[A] =
    wrap(Free.liftF(Lifted(r)))

  def kcas: Type[KCAS] =
    wrap(Free.liftF(ProvideKCAS))

  implicit final class AsyncReactOps[A](private val self: Type[A]) extends AnyVal {

    def runCancellable[F[_]](implicit kcas: KCAS, F: Async[F]): F[(F[Option[A]], F[Unit])] = {
      val k = unwrap(self).foldMap(interpreterCancellable[F])
      F.delay {
        val cancelRef = CancelRef.mk()
        val fma = k.value.run(cancelRef)
        val cancel = F.delay { cancelRef.cancel.unsafeRun }
        (fma, cancel)
      }
    }

    def run[F[_]](implicit kcas: KCAS, F: Async[F]): F[A] =
      unwrap(self).foldMap(interpreter[F])
  }

  implicit val asyncReactMonad: Monad[Type] =
    Free.catsFreeMonadForFree[ArOp].asInstanceOf[Monad[Type]]

  private sealed abstract class ArOp[A]

  private final case class Delay[A](f: () => A) extends ArOp[A]

  private final case class Lifted[A](r: React[Unit, A]) extends ArOp[A]

  private final case class RealAsync[A](r: (A => Unit) => Cancel) extends ArOp[A]

  private final case object ProvideKCAS extends ArOp[KCAS]

  private[async] sealed abstract class CancelRefResult
  private[async] sealed abstract class StubResult extends CancelRefResult
  private[async] final case object AlreadyCancelled extends StubResult
  private[async] final case object Ok extends StubResult
  private[async] final case object CannotModify extends CancelRefResult

  private[async] final class CancelRef(ref: Ref[Boolean], map: data.Map[Id, Option[Cancel]]) {

    private[this] val cancelled: Ref[Boolean] =
      ref

    /** None is a stub for future canceller */
    private[this] val cbs: data.Map[Id, Option[Cancel]] =
      map

    val isCancelled: React[Unit, Boolean] =
      cancelled.getter

    val cancel: React[Unit, Unit] = {
      (cancelled.modify(_ => true) × cbs.clear)
        .right
        .postCommit(React.lift { m => m.values.foreach(_.foreach(_(()))) })
        .discard
        .lmap[Unit](_ => ((), ()))
    }

    val addStub: React[Id, StubResult] =
      cancelled.guardNot(cbs.put.lmap[Id](_ -> None)).map(_.fold[StubResult](AlreadyCancelled)(_ => Ok))

    val addCanceller: React[(Id, Cancel), CancelRefResult] = {
      cancelled.guardNot(cbs.replace.lmap[(Id, Cancel)] { ic =>
        (ic._1, None, Some(ic._2))
      }).map(_.fold[CancelRefResult](AlreadyCancelled)(ok => if (ok) Ok else CannotModify))
    }

    val removeCanceller: React[Id, CancelRefResult] =
      cancelled.guardNot(cbs.del).map(_.fold[CancelRefResult](AlreadyCancelled)(x => if (x) Ok else CannotModify))
  }

  private[async] final object CancelRef {
    def mk(): CancelRef = new CancelRef(Ref.mk(false), data.Map.naive.unsafePerform(())(KCAS.NaiveKCAS))
  }

  private def isCancelled[F[_]](implicit kcas: KCAS, F: Sync[F]): OptionT[Kleisli[F, CancelRef, ?], Boolean] = {
    for {
      cancelRef <- OptionT.liftF(Kleisli.ask[F, CancelRef])
      cancelled <- OptionT.liftF[Kleisli[F, CancelRef, ?], Boolean](Kleisli.lift(F.delay { cancelRef.isCancelled.unsafeRun }))
    } yield cancelled
  }

  private def ifNotCancelled[F[_], A](act: F[A])(implicit kcas: KCAS, F: Sync[F]): OptionT[Kleisli[F, CancelRef, ?], A] = for {
    cancelled <- isCancelled[F]
    maybeA <- if (cancelled) {
      OptionT.none[Kleisli[F, CancelRef, ?], A]
    } else {
      OptionT.liftF[Kleisli[F, CancelRef, ?], A](Kleisli.lift[F, CancelRef, A](act))
    }
  } yield maybeA

  private def interpreterCancellable[F[_]](implicit kcas: KCAS, F: Async[F]): ArOp ~> OptionT[Kleisli[F, CancelRef, ?], ?] = {
    new ~>[ArOp, OptionT[Kleisli[F, CancelRef, ?], ?]] {
      def apply[A](op: ArOp[A]): OptionT[Kleisli[F, CancelRef, ?], A] = op match {
        case Delay(f) =>
          ifNotCancelled(F.delay { f() })
        case Lifted(r) =>
          ifNotCancelled(F.delay { r.unsafePerform(()) })
        case RealAsync(r) =>
          for {
            cancelRef <- OptionT.liftF(Kleisli.ask[F, CancelRef])
            cancelled <- isCancelled[F]
            a <- if (cancelled) {
              OptionT.none[Kleisli[F, CancelRef, ?], A]
            } else {
              OptionT[Kleisli[F, CancelRef, ?], A](Kleisli.lift[F, CancelRef, Option[A]](F.async[Option[A]] { cb =>
                val id = new Id
                cancelRef.addStub.unsafePerform(id) match {
                  case AlreadyCancelled =>
                    cb(Right(None))
                  case Ok =>
                    val cf = r { a =>
                      cancelRef.removeCanceller.unsafePerform(id) match {
                        case AlreadyCancelled =>
                          cb(Right(None))
                        case Ok =>
                          cb(Right(Some(a)))
                        case CannotModify =>
                          // we were called a second time, so we ignore it
                          ()
                      }
                    }
                    val canceller: Cancel = { _ =>
                      cf(())
                      cb(Right(None))
                    }
                    cancelRef.addCanceller.unsafePerform((id, canceller)) match {
                      case AlreadyCancelled =>
                        canceller(())
                      case Ok =>
                        ()
                      case CannotModify =>
                        // no stub found, which means it was already removed
                        ()
                    }
                }
              }))
            }
          } yield a
        case ProvideKCAS =>
          ifNotCancelled(F.pure(kcas))
      }
    }
  }

  private def interpreter[F[_]](implicit kcas: KCAS, F: Async[F]): ArOp ~> F = new ~>[ArOp, F] {
    def apply[A](op: ArOp[A]): F[A] = op match {
      case Delay(f) =>
        F.delay { f() }
      case Lifted(r) =>
        F.delay { r.unsafePerform(()) }
      case RealAsync(r) =>
        F.async { cb =>
          r { a => cb(Right(a)) }
          ()
        }
      case ProvideKCAS =>
        F.pure(kcas)
    }
  }
}
