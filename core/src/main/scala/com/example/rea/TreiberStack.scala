package com.example.rea

import kcas._

final class TreiberStack[A] {

  private[rea] val head = Ref.mk[List[A]](Nil)

  val push: React[A, Unit] = head.upd { (as, a) =>
    (a :: as, ())
  }

  val tryPop: React[Unit, Option[A]] = head.upd {
    case (h :: t, ()) => (t, Some(h))
    case (Nil, ()) => (Nil, None)
  }

  val length: React[Unit, Int] =
    head.upd[Unit, Int] { (l, _) => (l, l.length) }

  private[rea] def unsafeToList(implicit kcas: KCAS): List[A] = {
    val r = head.upd[Unit, List[A]] { (l, _) => (l, l) }
    r.run
  }
}
