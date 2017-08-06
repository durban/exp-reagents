package io.sigs.choam
package kcas

// TODO: detect impossible CAS-es
// TODO: support thread interruption in (some) retry loops
// TODO: think about exception safety (e.g., leaving behind descriptors)

/** Common interface for k-CAS implementations */
abstract class KCAS { self =>

  /**
   * Rules:
   * - no use after `tryPerform` or `cancel`
   * - must call `tryPerform` or `cancel` before releasing the reference
   */
  private[choam] trait Desc {
    final def impl: KCAS = self
    def withCAS[A](ref: Ref[A], ov: A, nv: A): Desc
    def snapshot(): Snap
    def tryPerform(): Boolean
    def cancel(): Unit
  }

  /**
   * Rules:
   * - mustn't `load` unless the original (which created the snapshot)
   *   is already finished (with `tryPerform` or `cancel`)
   */
  private[choam] trait Snap {
    def load(): Desc
  }

  private[choam] def start(): Desc

  private[choam] def tryReadOne[A](ref: Ref[A]): A

  @tailrec
  private[choam] final def read[A](ref: Ref[A]): A = {
    tryReadOne(ref) match {
      case null =>
        read(ref)
      case a =>
        a
    }
  }
}

/** Provides various k-CAS implementations */
private[choam] object KCAS {

  private[choam] lazy val NaiveKCAS: KCAS =
    kcas.NaiveKCAS

  private[choam] lazy val CASN: KCAS =
    kcas.CASN

  private[choam] lazy val MCAS: KCAS =
    kcas.MCAS

  def unsafeLookup(fqn: String): KCAS = fqn match {
    case fqns.NaiveKCAS =>
      NaiveKCAS
    case fqns.CASN =>
      CASN
    case fqns.MCAS =>
      MCAS
    case _ =>
      throw new IllegalArgumentException(fqn)
  }

  object fqns {
    final val NaiveKCAS =
      "io.sigs.choam.kcas.NaiveKCAS"
    final val CASN =
      "io.sigs.choam.kcas.CASN"
    final val MCAS =
      "io.sigs.choam.kcas.MCAS"
  }
}

// TODO: eliminate this (or preserve only as an impl detail of CASN)

/** CAS descriptor */
private[choam] sealed case class CASD[A](ref: Ref[A], ov: A, nv: A) {

  private[kcas] final def unsafeTryPerformOne(): Boolean =
    ref.unsafeTryPerformCas(ov, nv)

  final override def equals(that: Any): Boolean = that match {
    case CASD(tref, tov, tnv) =>
      (ref eq tref) && equ(ov, tov) && equ(nv, tnv)
    case _ =>
      false
  }

  final override def hashCode: Int =
    ref.## ^ System.identityHashCode(ov) ^ System.identityHashCode(nv)

  // TODO: implement total global order (to avoid deadlocks)
  private[kcas] final def globalRank: Int =
    ref.##
}

private[choam] object CASD {

  implicit val ordering: Ordering[CASD[_]] = new Ordering[CASD[_]] {
    override def compare(cx: CASD[_], cy: CASD[_]): Int = {
      val x = cx.globalRank
      val y = cy.globalRank
      if (x < y) -1
      else if (x > y) +1
      else 0
    }
  }
}
