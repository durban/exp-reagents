package io.sigs.choam
package kcas
package bench

final class Reset[A](a: A, refs: Ref[A]*) {

  private[this] val n = refs.size

  def reset(): Unit = {
    var i = 0
    while(i < n) {
      refs(i).unsafeSet(a)
      i += 1
    }
  }
}
