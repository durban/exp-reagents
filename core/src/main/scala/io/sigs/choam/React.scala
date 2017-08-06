package io.sigs.choam

import cats.Monad
import cats.arrow.Arrow
import cats.effect.Sync

import kcas._

/**
 * A *partial* implementation of reagents, described in [Reagents: Expressing and
 * Composing Fine-grained Concurrency](https://people.mpi-sws.org/~turon/reagents.pdf)
 * by Aaron Turon; originally implemented at [aturon/ChemistrySet](
 * https://github.com/aturon/ChemistrySet).
 *
 * This implementation is significantly simplified by the fact
 * that offers and permanent failure are not implemented. As a
 * consequence, these reactants are always non-blocking (provided
 * that the underlying k-CAS implementation is non-blocking).
 * However, this also means, that they are less featureful.
 *
 * @see Other implementations:
 *
 * - https://github.com/aturon/Caper (Racket)
 * - https://github.com/ocamllabs/reagents (OCaml)
 */
sealed abstract class React[-A, +B] {

  import React._

  final def unsafePerform(a: A)(implicit kcas: KCAS): B = {

    @tailrec
    def go[C](partialResult: C, cont: React[C, B], rea: Reaction, desc: KCAS#Desc, alts: List[SnapJump[_, B]]): Success[B] = {
      cont.tryPerform(maxStackDepth, partialResult, rea, desc) match {
        case Retry =>
          // TODO: implement backoff
          alts match {
            case _: Nil.type =>
              go(partialResult, cont, Reaction.empty, kcas.start(), alts)
            case SnapJump(pr, k, rea, snap) :: t =>
              // We're cheating here, to convince scalac
              // that this really is a tail-recursive call:
              go(pr.asInstanceOf[C], k, rea, snap.load(), t)
          }

        case s @ Success(_, _) =>
          s
        case Jump(pr, k, rea, desc, alts2) =>
          // We're cheating here, to convince scalac
          // that this really is a tail-recursive call:
          go(pr.asInstanceOf[C], k, rea, desc, alts2 ++ alts)
          // TODO: optimize concat
      }
    }

    val res = go(a, this, Reaction.empty, kcas.start(), Nil)
    res.reaction.postCommit.foreach { pc =>
      pc.unsafePerform(())
    }

    res.value
  }

  protected def tryPerform(n: Int, a: A, ops: Reaction, desc: KCAS#Desc): TentativeResult[B]

  protected final def maybeJump[C, Y >: B](n: Int, partialResult: C, cont: React[C, Y], ops: Reaction, desc: KCAS#Desc): TentativeResult[Y] = {
    if (n <= 0) Jump(partialResult, cont, ops, desc)
    else cont.tryPerform(n - 1, partialResult, ops, desc)
  }

  def + [X <: A, Y >: B](that: React[X, Y]): React[X, Y] =
    new Choice[X, Y](this, that)

  final def >>> [C](that: React[B, C]): React[A, C] = that match {
    case _: Commit.type => this
    case _ => this.andThenImpl(that)
  }

  protected def andThenImpl[C](that: React[B, C]): React[A, C]

  final def * [X <: A, C](that: React[X, C]): React[X, (B, C)] = {
    (this × that).lmap[X](x => (x, x))
  }

  final def × [C, D](that: React[C, D]): React[(A, C), (B, D)] =
    this.productImpl(that)

  final def ? : React[A, Option[B]] =
    this.rmap(Some(_)) + ret[Unit, Option[B]](None).lmap(_ => ())

  protected def productImpl[C, D](that: React[C, D]): React[(A, C), (B, D)]

  protected def firstImpl[C]: React[(A, C), (B, C)]

  final def lmap[X](f: X => A): React[X, B] =
    lift(f) >>> this

  final def rmap[C](f: B => C): React[A, C] =
    this >>> lift(f)

  final def map[C](f: B => C): React[A, C] =
    rmap(f)

  final def map2[X <: A, C, D](that: React[X, C])(f: (B, C) => D): React[X, D] =
    (this * that).map(f.tupled)

  final def discard: React[A, Unit] =
    this.rmap(_ => ()) // TODO: optimize

  final def flatMap[X <: A, C](f: B => React[X, C]): React[X, C] = {
    val self: React[X, (X, B)] = arrowInstance.second[X, B, X](this).lmap[X](x => (x, x))
    val comp: React[(X, B), C] = computed[(X, B), C](xb => f(xb._2).lmap[Unit](_ => xb._1))
    self >>> comp
  }

  private[choam] def postCommit(pc: React[B, Unit]): React[A, B] =
    this >>> React.postCommit(pc)

  override def toString: String
}

object React {

  private[choam] final val maxStackDepth = 1024

  def upd[A, B, C](r: Ref[A])(f: (A, B) => (A, C)): React[B, C] = {
    val self: React[B, (A, B)] = r.invisibleRead.firstImpl[B].lmap[B](b => ((), b))
    val comp: React[(A, B), C] = computed[(A, B), C] { case (oa, b) =>
      val (na, c) = f(oa, b)
      r.cas(oa, na).rmap(_ => c)
    }
    self >>> comp
  }

  def updWith[A, B, C](r: Ref[A])(f: (A, B) => React[Unit, (A, C)]): React[B, C] = {
    val self: React[B, (A, B)] = r.invisibleRead.firstImpl[B].lmap[B](b => ((), b))
    val comp: React[(A, B), C] = computed[(A, B), C] { case (oa, b) =>
      f(oa, b).flatMap {
        case (na, c) =>
          r.cas(oa, na).rmap(_ => c)
      }
    }
    self >>> comp
  }

  def computed[A, B](f: A => React[Unit, B]): React[A, B] =
    new Computed[A, B, B](f, Commit[B]())

  private[choam] def cas[A](r: Ref[A], ov: A, nv: A): React[Unit, Unit] =
    new Cas[A, Unit](r, ov, nv, lift(_ => ()))

  private[choam] def invisibleRead[A](r: Ref[A]): React[Unit, A] =
    new Read(r, Commit[A]())

  private[choam] def postCommit[A](pc: React[A, Unit]): React[A, A] =
    new PostCommit[A, A](pc, Commit[A]())

  def lift[A, B](f: A => B): React[A, B] =
    new Lift(f, Commit[B]())

  def identity[A]: React[A, A] =
    Commit[A]()

  private[this] val _unit =
    React.lift[Any, Unit] { _ => () }

  def unit[A]: React[A, Unit] =
    _unit

  def ret[X, A](a: A): React[X, A] =
    lift[X, A](_ => a)

  private[choam] def retry[A, B]: React[A, B] =
    AlwaysRetry()

  def newRef[A](initial: A): React[Unit, Ref[A]] =
    lift[Unit, Ref[A]](_ => Ref.mk(initial))

  implicit val arrowInstance: Arrow[React] = new Arrow[React] {

    def lift[A, B](f: A => B): React[A, B] =
      React.lift(f)

    def id[A]: React[A, A] =
      React.lift(a => a)

    def compose[A, B, C](f: React[B, C], g: React[A, B]): React[A, C] =
      g >>> f

    def first[A, B, C](fa: React[A, B]): React[(A, C), (B, C)] =
      fa.firstImpl

    override def lmap[A, B, X](fa: React[A, B])(f: X => A): React[X, B] =
      fa.lmap(f)

    override def rmap[A, B, C](fa: React[A, B])(f: B => C): React[A, C] =
      fa.rmap(f)
  }

  // TODO: MonadReader (?)
  implicit def monadInstance[X]: Monad[React[X, ?]] = new Monad[React[X, ?]] {

    def pure[A](x: A): React[X, A] =
      React.ret(x)

    def flatMap[A, B](fa: React[X, A])(f: A => React[X, B]): React[X, B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => React[X, Either[A, B]]): React[X, B] = {
      f(a).flatMap {
        case Left(a) => tailRecM(a)(f)
        case Right(b) => React.ret(b)
      }
    }
  }

  implicit final class InvariantReactSyntax[A, B](private val self: React[A, B]) extends AnyVal {
    final def apply[F[_]](a: A)(implicit kcas: KCAS, F: Sync[F]): F[B] =
      F.delay { self.unsafePerform(a)(kcas) }
  }

  implicit final class UnitReactSyntax[A](private val self: React[Unit, A]) extends AnyVal {

    final def run[F[_]](implicit kcas: KCAS, F: Sync[F]): F[A] =
      F.delay { unsafeRun(kcas) }

    // TODO: add ()
    final def unsafeRun(implicit kcas: KCAS): A =
      self.unsafePerform(())
  }

  // TODO: rename to PostCommit (?)
  // TODO: optimize building
  private final case class Reaction(
    postCommit: List[React[Unit, Unit]]
  ) {

    def :: (act: React[Unit, Unit]): Reaction =
      this.copy(postCommit = act :: postCommit)
  }

  private object Reaction {
    val empty: Reaction = Reaction(Nil)
  }

  protected[React] sealed trait TentativeResult[+A]
  protected[React] final case object Retry extends TentativeResult[Nothing]
  protected[React] final case class Success[A](value: A, reaction: Reaction) extends TentativeResult[A]
  protected[React] final case class Jump[A, B](
    value: A,
    react: React[A, B],
    ops: Reaction,
    desc: KCAS#Desc,
    alts: List[SnapJump[_, B]] = Nil,
  ) extends TentativeResult[B]

  protected[React] final case class SnapJump[A, B](
    value: A,
    react: React[A, B],
    ops: Reaction,
    snap: KCAS#Snap,
  )

  private sealed abstract class Commit[A]()
      extends React[A, A] {

    protected def tryPerform(n: Int, a: A, reaction: Reaction, desc: KCAS#Desc): TentativeResult[A] = {
      if (desc.tryPerform()) Success(a, reaction)
      else Retry
    }

    protected def andThenImpl[C](that: React[A, C]): React[A, C] =
      that

    protected def productImpl[C, D](that: React[C, D]): React[(A, C), (A, D)] = that match {
      case _: Commit.type => Commit[(A, C)]()
      case _ => arrowInstance.second(that) // TODO: optimize
    }

    def firstImpl[C]: React[(A, C), (A, C)] =
      Commit[(A, C)]()

    override def toString =
      "Commit"
  }

  private object Commit extends Commit[Any] {

    @inline
    def apply[A](): Commit[A] =
      this.asInstanceOf[Commit[A]]
  }

  private sealed abstract class AlwaysRetry[A, B]()
      extends React[A, B] {

    protected def tryPerform(n: Int, a: A, reaction: Reaction, desc: KCAS#Desc): TentativeResult[B] = {
      desc.cancel()
      Retry
    }

    protected def andThenImpl[C](that: React[B, C]): React[A, C] =
      AlwaysRetry()

    protected def productImpl[C, D](that: React[C, D]): React[(A, C), (B, D)] =
      AlwaysRetry()

    def firstImpl[C]: React[(A, C), (B, C)] =
      AlwaysRetry()

    override def toString =
      "Retry"
  }

  private object AlwaysRetry extends AlwaysRetry[Any, Any] {

    @inline
    def apply[A, B](): AlwaysRetry[A, B] =
      this.asInstanceOf[AlwaysRetry[A, B]]
  }

  private final class PostCommit[A, B](pc: React[A, Unit], k: React[A, B])
      extends React[A, B] {

    def tryPerform(n: Int, a: A, ops: Reaction, desc: KCAS#Desc): TentativeResult[B] =
      maybeJump(n, a, k, pc.lmap[Unit](_ => a) :: ops, desc)

    def andThenImpl[C](that: React[B, C]): React[A, C] =
      new PostCommit[A, C](pc, k >>> that)

    def productImpl[C, D](that: React[C, D]): React[(A, C), (B, D)] =
      new PostCommit[(A, C), (B, D)](pc.lmap[(A, C)](_._1), k × that)

    def firstImpl[C]: React[(A, C), (B, C)] =
      new PostCommit[(A, C), (B, C)](pc.lmap[(A, C)](_._1), k.firstImpl[C])

    override def toString =
      s"PostCommit(${pc}, ${k})"
  }

  private final class Lift[A, B, C](private val func: A => B, private val k: React[B, C])
      extends React[A, C] {

    def tryPerform(n: Int, a: A, ops: Reaction, desc: KCAS#Desc): TentativeResult[C] =
      maybeJump(n, func(a), k, ops, desc)

    def andThenImpl[D](that: React[C, D]): React[A, D] = {
      new Lift[A, B, D](func, k >>> that)
    }

    def productImpl[D, E](that: React[D, E]): React[(A, D), (C, E)] = that match {
      case _: Commit[_] =>
        new Lift[(A, D), (B, D), (C, D)](ad => (func(ad._1), ad._2), k.firstImpl)
      case l: Lift[D, x, E] =>
        new Lift[(A, D), (B, x), (C, E)](ad => (func(ad._1), l.func(ad._2)), k × l.k)
      case _ =>
        new Lift(ad => (func(ad._1), ad._2), k × that)
    }

    def firstImpl[D]: React[(A, D), (C, D)] =
      new Lift[(A, D), (B, D), (C, D)](ad => (func(ad._1), ad._2), k.firstImpl[D])

    override def toString =
      s"Lift(<function>, ${k})"
  }

  private final class Computed[A, B, C](f: A => React[Unit, B], k: React[B, C])
      extends React[A, C] {

    def tryPerform(n: Int, a: A, ops: Reaction, desc: KCAS#Desc): TentativeResult[C] =
      maybeJump(n, (), f(a) >>> k, ops, desc)

    def andThenImpl[D](that: React[C, D]): React[A, D] = {
      new Computed(f, k >>> that)
    }

    def productImpl[D, E](that: React[D, E]): React[(A, D), (C, E)] = {
      new Computed[(A, D), (B, D), (C, E)](
        ad => f(ad._1).rmap(b => (b, ad._2)),
        k × that
      )
    }

    def firstImpl[D]: React[(A, D), (C, D)] = {
      new Computed[(A, D), (B, D), (C, D)](
        ad => f(ad._1).firstImpl[D].lmap[Unit](_ => ((), ad._2)),
        k.firstImpl[D]
      )
    }

    override def toString =
      s"Computed(<function>, ${k})"
  }

  private final class Choice[A, B](first: React[A, B], second: React[A, B])
      extends React[A, B] {

    def tryPerform(n: Int, a: A, ops: Reaction, desc: KCAS#Desc): TentativeResult[B] = {
      if (n <= 0) {
        Jump(a, this, ops, desc) // FIXME!!!
      } else {
        val snap = desc.snapshot()
        first.tryPerform(n - 1, a, ops, desc) match {
          case Retry =>
            second.tryPerform(n - 1, a, ops, snap.load())
          case Jump(x, r, o, d, as) =>
            // TODO: optimize building `alts`
            Jump(x, r, o, d, as :+ SnapJump(a, second, ops, snap))
          case ok =>
            ok
        }
      }
    }

    def andThenImpl[C](that: React[B, C]): React[A, C] =
      new Choice[A, C](first >>> that, second >>> that)

    def productImpl[C, D](that: React[C, D]): React[(A, C), (B, D)] =
      new Choice[(A, C), (B, D)](first × that, second × that)

    def firstImpl[C]: React[(A, C), (B, C)] =
      new Choice[(A, C), (B, C)](first.firstImpl, second.firstImpl)

    override def toString =
      s"Choice(${first}, ${second})"
  }

  private abstract class GenCas[A, B, C, D](ref: Ref[A], ov: A, nv: A, k: React[C, D])
      extends React[B, D] { self =>

    /** Must be pure */
    protected def transform(a: A, b: B): C

    protected def tryPerform(n: Int, b: B, pc: Reaction, desc: KCAS#Desc): TentativeResult[D] = {
      desc.impl.tryReadOne(ref) match {
        case null =>
          desc.cancel()
          Retry
        case a if equ(a, ov) =>
          maybeJump(n, transform(a, b), k, pc, desc.withCAS(ref, ov, nv))
        case _ =>
          desc.cancel()
          Retry
      }
    }

    def andThenImpl[E](that: React[D, E]): React[B, E] = {
      new GenCas[A, B, C, E](ref, ov, nv, k >>> that) {
        protected def transform(a: A, b: B): C =
          self.transform(a, b)
      }
    }

    def productImpl[E, F](that: React[E, F]): React[(B, E), (D, F)] = {
      new GenCas[A, (B, E), (C, E), (D, F)](ref, ov, nv, k × that) {
        protected def transform(a: A, be: (B, E)): (C, E) =
          (self.transform(a, be._1), be._2)
      }
    }

    def firstImpl[E]: React[(B, E), (D, E)] = {
      new GenCas[A, (B, E), (C, E), (D, E)](ref, ov, nv, k.firstImpl[E]) {
        protected def transform(a: A, be: (B, E)): (C, E) =
          (self.transform(a, be._1), be._2)
      }
    }

    override def toString =
      s"GenCas(${ref}, ${ov}, ${nv}, ${k})"
  }

  private final class Cas[A, B](ref: Ref[A], ov: A, nv: A, k: React[A, B])
      extends GenCas[A, Unit, A, B](ref, ov, nv, k) {

    def transform(a: A, b: Unit): A =
      a
  }

  private abstract class GenRead[A, B, C, D](ref: Ref[A], k: React[C, D])
      extends React[B, D] { self =>

    /** Must be pure */
    protected def transform(a: A, b: B): C

    protected def tryPerform(n: Int, b: B, ops: Reaction, desc: KCAS#Desc): TentativeResult[D] = {
      desc.impl.tryReadOne(ref) match {
        case null => Retry
        case a => maybeJump(n, transform(a, b), k, ops, desc)
      }
    }

    def andThenImpl[E](that: React[D, E]): React[B, E] = {
      new GenRead[A, B, C, E](ref, k >>> that) {
        protected def transform(a: A, b: B): C =
          self.transform(a, b)
      }
    }

    def productImpl[E, F](that: React[E, F]): React[(B, E), (D, F)] = {
      new GenRead[A, (B, E), (C, E), (D, F)](ref, k × that) {
        protected def transform(a: A, be: (B, E)): (C, E) =
          (self.transform(a, be._1), be._2)
      }
    }

    def firstImpl[E]: React[(B, E), (D, E)] = {
      new GenRead[A, (B, E), (C, E), (D, E)](ref, k.firstImpl[E]) {
        protected def transform(a: A, be: (B, E)): (C, E) =
          (self.transform(a, be._1), be._2)
      }
    }

    override def toString =
      s"GenRead(${ref}, ${k})"
  }

  private final class Read[A, B](ref: Ref[A], k: React[A, B])
      extends GenRead[A, Unit, A, B](ref, k) {

    def transform(a: A, b: Unit): A =
      a
  }
}
