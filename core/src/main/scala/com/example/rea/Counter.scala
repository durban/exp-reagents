package com.example.rea

import kcas.Ref

final class Counter {

  private[this] val ref =
    Ref.mk(0L)

  val add: React[Long, Long] = ref.upd[Long, Long] {
    case (cnt, n) => (cnt + n, cnt)
  }

  val incr: React[Unit, Long] =
    add.lmap(_ => 1L)

  val decr: React[Unit, Long] =
    add.lmap(_ => -1L)

  val count: React[Unit, Long] =
    add.lmap(_ => 0L)
}
