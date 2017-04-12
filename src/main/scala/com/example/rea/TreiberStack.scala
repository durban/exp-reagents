package com.example.rea

import kcas.Ref

final class TreiberStack[A] {

  private[rea] val head = Ref.mk[List[A]](Nil)

  val push: React[A, Unit] = head.upd { (as, a) =>
    (a :: as, ())
  }

  val tryPop: React[Unit, Option[A]] = head.upd {
    case (h :: t, ()) => (t, Some(h))
    case (Nil, ()) => (Nil, None)
  }

  private[rea] def unsafeToList: List[A] = {
    val r = head.upd[Unit, List[A]] { (l, _) => (l, l) }
    r.run
  }
}
