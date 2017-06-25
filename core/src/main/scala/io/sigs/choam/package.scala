package io.sigs

package object choam {

  private[choam] type tailrec = scala.annotation.tailrec

  @inline
  private[choam] def box[A](a: A): AnyRef =
    a.asInstanceOf[AnyRef]

  @inline
  private[choam] def equ[A](x: A, y: A): Boolean =
    box(x) eq box(y)

  @inline
  private[choam] def nullOf[A]: A =
    null.asInstanceOf[A]

  private[choam] def impossible(s: String): Nothing =
    throw new IllegalStateException(s)
}
