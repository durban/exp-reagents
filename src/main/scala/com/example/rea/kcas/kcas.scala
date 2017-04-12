package com.example.rea
package kcas

import java.util.concurrent.atomic.AtomicReference

/** Common interface for k-CAS implementations */
private[rea] trait KCAS {
  def tryPerform(ops: List[CASD[_]]): Boolean
  def tryReadOne[A](ref: Ref[A]): A
}

/** Selects a k-CAS implementation based on a system property */
private[rea] object KCAS {

  private[rea] def impl: KCAS =
    this.implHolder.get()

  private[this] val implHolder: AtomicReference[KCAS] = {
    val impl = System.getProperty("com.example.rea.kcas.impl") match {
      case null | "" =>
        // fall back to default:
        CASN
      case "com.example.rea.kcas.CASN" =>
        CASN
      case "com.example.rea.kcas.NaiveKCAS" =>
        NaiveKCAS
      case x =>
        throw new IllegalArgumentException(s"Invalid kCAS impl: '${x}'")
    }
    new AtomicReference[KCAS](impl)
  }
}

/** CAS descriptor */
private[rea] sealed case class CASD[A](ref: Ref[A], ov: A, nv: A) {

  private[kcas] def unsafeTryPerformOne(): Boolean =
    ref.unsafeTryPerformCas(ov, nv)

  final override def equals(that: Any): Boolean = that match {
    case CASD(tref, tov, tnv) =>
      (ref eq tref) && equ(ov, tov) && equ(nv, tnv)
    case _ =>
      false
  }

  final override def hashCode: Int =
    System.identityHashCode(this)
}

/**
 * An implementation of [A Practical Multi-Word Compare-and-Swap Operation](
 * http://www.cl.cam.ac.uk/research/srg/netos/papers/2002-casn.pdf) by
 * Timothy L. Harris, Keir Fraser and Ian A. Pratt.
 */
private[kcas] object CASN extends KCAS {

  override def tryPerform(ops: List[CASD[_]]): Boolean =
    CASN(CASNDesc(ops))

  override def tryReadOne[A](ref: Ref[A]): A =
    CASNRead(ref)

  private[kcas] final case class RDCSSDesc[A1, A2](
    a1: Ref[A1],
    o1: A1,
    a2: Ref[A2],
    o2: A2,
    n2: A2
  ) {

    private[CASN] def as[A]: A =
      this.asInstanceOf[A]
  }

  private[kcas] sealed trait RDCSSResult
  private[kcas] final case object AcquireSuccess extends RDCSSResult
  private[kcas] final case object AcquireFailure extends RDCSSResult
  private[kcas] final case class OtherDescriptor(cd: CASNDesc) extends RDCSSResult

  private def CAS1toDesc[A](r: Ref[A], ov: A, nv: RDCSSDesc[_, A]): Boolean = {
    r.unsafeTryPerformCas(ov, nv.as[A])
  }

  private def CAS1fromDesc[A](r: Ref[A], ov: RDCSSDesc[_, A], nv: A): Boolean = {
    r.unsafeTryPerformCas(ov.as[A], nv)
  }

  private[kcas] def RDCSS[A1, A2](d: RDCSSDesc[A1, A2]): RDCSSResult = {
    @tailrec
    def acquire(): RDCSSResult = {
      if (CAS1toDesc(d.a2, d.o2, d)) {
        // ok, we succeeded:
        AcquireSuccess
      } else {
        // we failed ...
        d.a2.unsafeTryRead() match {
          case r @ RDCSSDesc(_, _, _, _, _) =>
            // other op underway, let's help:
            RDCSSComp(r)
            // and retry ours:
            acquire()
          case cd @ CASNDesc(_) =>
            OtherDescriptor(cd)
          case x =>
            // probably other op completed before us:
            AcquireFailure
        }
      }
    }

    val res = acquire()
    res match {
      case AcquireSuccess =>
        RDCSSComp(d)
      case _ =>
        // `acquire` failed, no need
        // to call `RDCSSComp`, it
        // would fail for sure
        ()
    }

    res
  }

  private[kcas] def RDCSSRead[A](ref: Ref[A]): A = {
    @tailrec
    def go(): A = {
      val r: A = ref.unsafeTryRead()
      (r : Any) match {
        case r @ RDCSSDesc(_, _, _, _, _) =>
          // help the other thread, then retry:
          RDCSSComp(r)
          go()
        case _ =>
          // ok:
          r
      }
    }

    go()
  }

  private def RDCSSComp[A1, A2](d: RDCSSDesc[A1, A2]): Unit = {
    val v: A1 = d.a1.unsafeTryRead()
    if (equ(v, d.o1)) {
      CAS1fromDesc(d.a2, d, d.n2)
    } else {
      CAS1fromDesc(d.a2, d, d.o2)
    }
  }

  private sealed trait CASNStatus
  private final case object Undecided extends CASNStatus
  private sealed trait Decided extends CASNStatus
  private final case object Failed extends Decided
  private final case object Succeeded extends Decided

  // TODO: sort entries
  private[kcas] final case class CASNDesc(entries: List[CASD[_]]) {

    private[CASN] val status: Ref[CASNStatus] =
      Ref.mk(Undecided)

    private[CASN] def as[A]: A =
      this.asInstanceOf[A]
  }

  private def RDCSStoDesc[A](
    status: Ref[CASNStatus],
    expStatus: CASNStatus,
    ref: Ref[A],
    ov: A,
    nv: CASNDesc
  ): RDCSSResult = RDCSS(RDCSSDesc(status, expStatus, ref, ov, nv.as[A]))

  private def CAS1fromDesc[A](ref: Ref[A], ov: CASNDesc, nv: A): Boolean = {
    ref.unsafeTryPerformCas(ov.as[A], nv)
  }

  private[kcas] def CASN(cd: CASNDesc): Boolean = {
    // Phase 1 starts from UNDECIDED:
    if (cd.status.unsafeTryRead() eq Undecided) {
      @tailrec
      def phase1(entries: List[CASD[_]]): Decided = {
        entries match {
          case Nil =>
            Succeeded
          case (entry: CASD[a]) :: tail =>
            val res = RDCSStoDesc[a](cd.status, Undecided, entry.ref, entry.ov, cd)
            res match {
              case OtherDescriptor(desc) =>
                if (!equ(desc, cd)) {
                  // help the other op:
                  CASN(desc)
                  // and retry from this entry:
                  phase1(entries)
                } else {
                  // somebody helped us, continue:
                  phase1(tail)
                }
              case AcquireFailure =>
                // other op succeeded:
                Failed
              case AcquireSuccess =>
                // continue:
                phase1(tail)
            }
        }
      }

      val status: Decided = phase1(cd.entries)
      cd.status.unsafeTryPerformCas(Undecided, status)
    }

    // Phase 2 (now status is either FAILED or SUCCEEDED):
    val succeeded = equ(cd.status.unsafeTryRead(), Succeeded)

    @tailrec
    def phase2(entries: List[CASD[_]]): Boolean = {
      entries match {
        case Nil =>
          // we're done:
          succeeded
        case (entry: CASD[a]) :: tail =>
          CAS1fromDesc[a](entry.ref, cd, if (succeeded) entry.nv else entry.ov)
          phase2(tail)
      }
    }

    phase2(cd.entries)
  }

  @tailrec
  private[kcas] def CASNRead[A](ref: Ref[A]): A = {
    val r = RDCSSRead(ref)
    r match {
      case d @ CASNDesc(_) =>
        // help the other op:
        CASN(d)
        // and retry:
        CASNRead(ref)
      case _ =>
        // ok, we found it:
        r
    }
  }
}

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
private[kcas] object NaiveKCAS extends KCAS {

  def tryReadOne[A](ref: Ref[A]): A =
    ref.unsafeTryRead()

  def tryPerform(ops: List[CASD[_]]): Boolean = {

    // TODO: sort list

    @tailrec
    def lock(ops: List[CASD[_]]): List[CASD[_]] = ops match {
      case Nil =>
        Nil
      case CASD(ref, ov, nv) :: tail =>
        if (ref.unsafeTryPerformCas(ov, null)) lock(tail)
        else ops // rollback
    }

    @tailrec
    def commit(ops: List[CASD[_]]): Unit = ops match {
      case Nil =>
        ()
      case CASD(ref, ov, nv) :: tail =>
        ref.unsafeLazySet(nv)
        commit(tail)
    }

    @tailrec
    def rollback(from: List[CASD[_]], to: List[CASD[_]]): Unit = {
      if (from ne to) {
        from match {
          case Nil =>
            impossible("this is the end")
          case CASD(ref, ov, nv) :: tail =>
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
      case _ :: _ =>
        lock(ops) match {
          case Nil =>
            commit(ops)
            true
          case to @ (_ :: _) =>
            rollback(ops, to)
            false
        }
    }
  }
}
