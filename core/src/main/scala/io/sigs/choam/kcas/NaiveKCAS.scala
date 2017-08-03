package io.sigs.choam
package kcas

/**
 * Naïve k-CAS algorithm as described in [Reagents: Expressing and Composing
 * Fine-grained Concurrency](https://people.mpi-sws.org/~turon/reagents.pdf)
 * by Aaron Turon; originally implemented at [aturon/ChemistrySet](
 * https://github.com/aturon/ChemistrySet).
 *
 * While this is logically correct, it basically implements
 * a spinlock for each `Ref`. Thus, it is not lock-free.
 *
 * Implemented as a baseline for benchmarking and correctness tests.
 */
private[kcas] object NaiveKCAS extends KCAS { self =>

  final case class DescRepr(ops: List[CASD[_]]) extends self.Desc with self.Snap {

    override def tryPerform(): Boolean =
      perform(ops.sorted)

    override def cancel(): Unit =
      ()

    override def withCAS[A](ref: Ref[A], ov: A, nv: A): self.Desc =
      copy(ops = CASD(ref, ov, nv) :: ops)

    override def snapshot(): self.Snap =
      this

    override def load(): self.Desc =
      this
  }

  override def start(): self.Desc =
    DescRepr(Nil)

  override def tryReadOne[A](ref: Ref[A]): A =
    ref.unsafeTryRead()

  private def perform(ops: List[CASD[_]]): Boolean = {

    @tailrec
    def lock(ops: List[CASD[_]]): List[CASD[_]] = ops match {
      case Nil =>
        Nil
      case CASD(ref, ov, _) :: tail =>
        if (ref.unsafeTryPerformCas(ov, null)) lock(tail)
        else ops // rollback
    }

    @tailrec
    def commit(ops: List[CASD[_]]): Unit = ops match {
      case Nil =>
        ()
      case CASD(ref, _, nv) :: tail =>
        ref.unsafeLazySet(nv)
        commit(tail)
    }

    @tailrec
    def rollback(from: List[CASD[_]], to: List[CASD[_]]): Unit = {
      if (from ne to) {
        from match {
          case Nil =>
            impossible("this is the end")
          case CASD(ref, ov, _) :: tail =>
            ref.unsafeLazySet(ov)
            rollback(tail, to)
        }
      } else {
        ()
      }
    }

    ops match {
      case Nil =>
        true
      case h :: Nil =>
        h.unsafeTryPerformOne()
      case l @ (_ :: _) =>
        lock(l) match {
          case Nil =>
            commit(l)
            true
          case to @ (_ :: _) =>
            rollback(l, to)
            false
        }
    }
  }
}
