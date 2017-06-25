package io.sigs.choam
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

  def kOf(d: Desc): Int = {
    val f = d.getClass.getDeclaredField("k")
    f.setAccessible(true)
    f.getInt(d)
  }

  type Entry = {
    def ref: Ref[_]
    def next: Any
  }

  def getTlStK(): Int = {
    val cls = Class.forName("io.sigs.choam.kcas.MCAS$TlSt$")
    val mod = cls.getDeclaredField("MODULE$")
    mod.setAccessible(true)
    val inst = cls.getDeclaredField("inst")
    inst.setAccessible(true)
    val tl = inst.get(mod.get(null)).asInstanceOf[ThreadLocal[_]]
    val obj = tl.get()
    val kf = obj.getClass().getDeclaredField("maxK")
    kf.setAccessible(true)
    kf.getInt(obj)
  }

  "MCAS" should "sort the entries before performing the CAS" in {
    val N = 20
    val refs = List.fill(N)(Ref.mk[String]("s"))
    val desc = MCAS.start().asInstanceOf[MCAS.Desc with Desc]
    desc.incr() // to avoid releasing it after `perform`
    refs.foldLeft(0) { (k, ref) =>
      kOf(desc) should === (k)
      desc.withCAS(ref, "s", "t")
      k + 1
    }
    kOf(desc) should === (N)
    assert(desc.tryPerform())
    kOf(desc) should === (N)

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
    kOf(desc) should === (0)
  }

  it should "save the size of the performed CAS to thread-local state" in {
    val N = 30
    val refs = List.fill(N)(Ref.mk[String]("s"))
    val desc = MCAS.start().asInstanceOf[MCAS.Desc with Desc]
    for (ref <- refs) {
      desc.withCAS(ref, "s", "t")
    }
    assert(desc.tryPerform())
    getTlStK() should === (N)
    kOf(desc) should === (0)
  }

  it should "calculate a correct K when saving/loading snapshots" in {
    val N = 5
    val refs = List.fill(N)(Ref.mk[String]("s"))
    val desc = MCAS.start().asInstanceOf[MCAS.Desc with Desc]
    kOf(desc) should === (0)
    desc.withCAS(refs(0), "s", "t")
    desc.withCAS(refs(1), "s", "t")
    desc.withCAS(refs(2), "s", "t")
    kOf(desc) should === (3)
    val snap = desc.snapshot()
    kOf(desc) should === (3)
    desc.withCAS(refs(3), "s", "t")
    desc.withCAS(refs(4), "s", "t")
    kOf(desc) should === (5)
    val desc2 = snap.load().asInstanceOf[MCAS.Desc with Desc]
    kOf(desc) should === (5)
    kOf(desc2) should === (3)
    assert(desc.tryPerform())
    kOf(desc) should === (0)
    kOf(desc2) should === (3)
    assert(!desc2.tryPerform())
    kOf(desc) should === (0)
    kOf(desc2) should === (0)
  }
}
