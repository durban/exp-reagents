package com.example.rea

import scala.concurrent.ExecutionContext

import org.scalatest.{ FlatSpecLike, Matchers }
import org.scalactic.TypeCheckedTripleEquals

trait KCASImplSpec {
  implicit def kcasImpl: kcas.KCAS
}

trait BaseSpec extends KCASImplSpec with FlatSpecLike with Matchers with TypeCheckedTripleEquals {

  implicit val ec: ExecutionContext =
    ExecutionContext.global
}

trait SpecNaiveKCAS extends KCASImplSpec {
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.NaiveKCAS
}

trait SpecCASN extends KCASImplSpec {
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.CASN
}

trait SpecMCAS extends KCASImplSpec {
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.MCAS
}
