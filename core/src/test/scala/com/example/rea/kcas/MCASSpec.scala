package com.example.rea
package kcas

import org.scalatest.{ FlatSpec, Matchers }
import org.scalactic.TypeCheckedTripleEquals

class MCASSpec extends FlatSpec with Matchers with TypeCheckedTripleEquals {

  // We need this here to access internals:
  import scala.language.reflectiveCalls

  type Desc = {
    def incr(): Unit
    def decr(): Unit
  }

  def headOf(d: Desc): Entry = {
    val f = d.getClass.getDeclaredField("head")
    f.setAccessible(true)
    f.get(d).asInstanceOf[Entry]
  }

  type Entry = {
    def ref: Ref[_]
    def next: Any
  }

  "MCAS" should "sort the entries before performing the CAS" in {
    val refs = List.fill(20)(Ref.mk[String]("s"))
    val desc = MCAS.start().asInstanceOf[MCAS.Desc with Desc]
    desc.incr() // to avoid releasing it after `perform`
    for (ref <- refs) {
      desc.withCAS(ref, "s", "t")
    }
    assert(desc.tryPerform())

    @tailrec
    def extract(h: Entry, acc: List[Ref[String]]): List[Ref[String]] = {
      if (h eq null) {
        acc
      } else {
        extract(h.next.asInstanceOf[Entry], h.ref.asInstanceOf[Ref[String]] :: acc)
      }
    }

    val sortedRefs = extract(headOf(desc), Nil)
    sortedRefs should === (refs.sortBy(_.##).reverse)

    desc.decr()
  }
}
