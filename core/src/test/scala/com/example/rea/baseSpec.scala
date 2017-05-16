package com.example.rea

import org.scalatest.{ FlatSpecLike, Matchers }
import org.scalactic.TypeCheckedTripleEquals

trait BaseSpec extends FlatSpecLike with Matchers with TypeCheckedTripleEquals {
  implicit def kcasImpl: kcas.KCAS
}

trait SpecNaiveKCAS { this: BaseSpec =>
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.NaiveKCAS
}

trait SpecCASN { this: BaseSpec =>
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.CASN
}
