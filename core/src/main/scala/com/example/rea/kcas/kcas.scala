package com.example.rea
package kcas

// TODO: detect impossible CAS-es
// TODO: support thread interruption in  (some) retry loops
// TODO: think about exception safety (e.g., leaving behind descriptors)

/** Common interface for k-CAS implementations */
private[rea] abstract class KCAS { self =>

  trait Desc {
    final def impl: KCAS = self
    def withCAS[A](ref: Ref[A], ov: A, nv: A): Desc
    def snapshot(): Snap
    def tryPerform(): Boolean
  }

  trait Snap {
    def load(): Desc
  }

  def start(): Desc

  def tryReadOne[A](ref: Ref[A]): A

  private[rea] final def tryPerformBatch(ops: List[CASD[_]]): Boolean = {
    val desc = ops.foldLeft(this.start()) { (d, op) =>
      op match {
        case op: CASD[a] =>
          d.withCAS[a](op.ref, op.ov, op.nv)
      }
    }
    desc.tryPerform()
  }
}

/** Provides various k-CAS implementations */
private[rea] object KCAS {

  private[rea] lazy val NaiveKCAS: KCAS =
    kcas.NaiveKCAS

  private[rea] lazy val CASN: KCAS =
    kcas.CASN

  private[rea] lazy val MCAS: KCAS =
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
      "com.example.rea.kcas.NaiveKCAS"
    final val CASN =
      "com.example.rea.kcas.CASN"
    final val MCAS =
      "com.example.rea.kcas.MCAS"
  }
}

// TODO: eliminate this (or preserve only as an impl detail of CASN)

/** CAS descriptor */
private[rea] sealed case class CASD[A](ref: Ref[A], ov: A, nv: A) {

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

private[rea] object CASD {

  val ordering: Ordering[CASD[_]] = new Ordering[CASD[_]] {
    override def compare(cx: CASD[_], cy: CASD[_]): Int = {
      val x = cx.globalRank
      val y = cy.globalRank
      if (x < y) -1
      else if (x > y) +1
      else 0
    }
  }
}
