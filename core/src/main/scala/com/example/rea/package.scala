package com.example

package object rea {

  private[rea] type tailrec = scala.annotation.tailrec

  private[rea] def box[A](a: A): AnyRef =
    a.asInstanceOf[AnyRef]

  private[rea] def equ[A](x: A, y: A): Boolean =
    box(x) eq box(y)

  private[rea] def nullOf[A]: A =
    null.asInstanceOf[A]

  private[rea] def impossible(s: String): Nothing =
    throw new IllegalStateException(s)
}
