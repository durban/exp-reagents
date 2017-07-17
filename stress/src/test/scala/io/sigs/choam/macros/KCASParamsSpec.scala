package io.sigs.choam
package macros

import org.scalatest.{ FlatSpec, Matchers }
import org.scalactic.TypeCheckedTripleEquals

import org.openjdk.jcstress.annotations._
import org.openjdk.jcstress.infra.results.ZZZ_Result

import kcas.KCAS

class KCASParamsSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  "KCASParams macro" should "generate the parameterized subclasses" in {
    val sub1 = new DummyTest.DummyTestCASN
    sub1.kcasImplPublic shouldBe theSameInstanceAs (KCAS.CASN)
    val sub2 = new DummyTest.DummyTestMCAS
    sub2.kcasImplPublic shouldBe theSameInstanceAs (KCAS.MCAS)
    val sub3 = new DummyTest.DummyTestNaiveKCAS
    sub3.kcasImplPublic shouldBe theSameInstanceAs (KCAS.NaiveKCAS)
    for (sub <- List(sub1, sub2, sub3)) {
      sub
        .getClass()
        .getDeclaredMethod("actor1", classOf[ZZZ_Result])
        .getDeclaredAnnotations()
        .apply(0)
        .annotationType() shouldBe theSameInstanceAs (classOf[Actor])
      sub
        .getClass()
        .getDeclaredMethod("arbiter", classOf[ZZZ_Result])
        .getDeclaredAnnotations()
        .apply(0)
        .annotationType() shouldBe theSameInstanceAs (classOf[Arbiter])

      intercept[NoSuchMethodException] {
        sub
          .getClass()
          .getDeclaredMethod("foo", classOf[Int])
      }
    }
  }
}
