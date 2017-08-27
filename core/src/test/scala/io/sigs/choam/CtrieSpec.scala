package io.sigs.choam

import cats.Eq
import cats.implicits._
import org.scalatest.prop.GeneratorDrivenPropertyChecks

abstract class CtrieSpec extends BaseSpec with GeneratorDrivenPropertyChecks {

  val hs: Int => Int = { x => x }

  def newEmpty(
    hashFunc: Int => Int = hs,
    eqFunc: (Int, Int) => Boolean = _ == _
  ): Ctrie[Int, String] = {
    new Ctrie[Int, String](hashFunc, Eq.instance(eqFunc))
  }

  "Ctrie#lookup" should "not find anything in an empty trie" in {
    val ct = newEmpty()
    forAll { i: Int =>
      ct.lookup.unsafePerform(i) should === (None)
    }
  }

  it should "find a previously inserted single key" in {
    forAll { (k: Int, i: Int) =>
      val ct = newEmpty()
      ct.insert.unsafePerform(k -> k.toString)
      ct.lookup.unsafePerform(k) should === (Some(k.toString))
      if (i =!= k) {
        ct.lookup.unsafePerform(i) should === (None)
      }
    }
  }

  it should "find all previously inserted keys" in {
    forAll { (ks: Set[Int], x: Int) =>
      val ct = newEmpty()
      val shadow = new scala.collection.mutable.HashSet[Int]
      for (k <- ks) {
        ct.insert.unsafePerform(k -> k.toString)
        shadow += k
        for (i <- shadow) {
          ct.lookup.unsafePerform(i) should === (Some(i.toString))
        }
        if (!shadow.contains(x)) {
          ct.lookup.unsafePerform(x) should === (None)
        }
      }
    }
  }

  it should "find an equal key which is not equal according to universal equality" in {
    val ct = newEmpty(_ % 4, (x, y) => (x % 8) == (y % 8))
    ct.insert.unsafePerform(0 -> "0")
    ct.lookup.unsafePerform(0) should === (Some("0"))
    ct.lookup.unsafePerform(8) should === (Some("0"))
    ct.insert.unsafePerform(4 -> "4")
    ct.lookup.unsafePerform(0) should === (Some("0"))
    ct.lookup.unsafePerform(8) should === (Some("0"))
    ct.lookup.unsafePerform(4) should === (Some("4"))
    ct.lookup.unsafePerform(12) should === (Some("4"))
  }

  "Ctrie#insert" should "handle hash collisions correctly" in {
    forAll { (ks: Set[Int], x: Int) =>
      val ct = newEmpty(_ % 8)
      ct.insert.unsafePerform(x -> x.toString)
      ct.lookup.unsafePerform(x) should === (Some(x.toString))
      ct.insert.unsafePerform(x + 8 -> (x + 8).toString)
      ct.lookup.unsafePerform(x) should === (Some(x.toString))
      ct.lookup.unsafePerform(x + 8) should === (Some((x + 8).toString))
      ct.insert.unsafePerform(x + 16 -> (x + 16).toString)
      ct.lookup.unsafePerform(x) should === (Some(x.toString))
      ct.lookup.unsafePerform(x + 8) should === (Some((x + 8).toString))
      ct.lookup.unsafePerform(x + 16) should === (Some((x + 16).toString))
      ct.insert.unsafePerform(x + 1 -> (x + 1).toString)
      ct.lookup.unsafePerform(x) should === (Some(x.toString))
      ct.lookup.unsafePerform(x + 8) should === (Some((x + 8).toString))
      ct.lookup.unsafePerform(x + 16) should === (Some((x + 16).toString))
      ct.lookup.unsafePerform(x + 1) should === (Some((x + 1).toString))
      ct.insert.unsafePerform(x + 9 -> (x + 9).toString)
      ct.lookup.unsafePerform(x) should === (Some(x.toString))
      ct.lookup.unsafePerform(x + 8) should === (Some((x + 8).toString))
      ct.lookup.unsafePerform(x + 16) should === (Some((x + 16).toString))
      ct.lookup.unsafePerform(x + 1) should === (Some((x + 1).toString))
      ct.lookup.unsafePerform(x + 9) should === (Some((x + 9).toString))
      for (k <- ks) {
        ct.insert.unsafePerform(k -> k.toString)
        ct.lookup.unsafePerform(k) should === (Some(k.toString))
      }
      ct.insert.unsafePerform(x + 17 -> (x + 17).toString)
      ct.lookup.unsafePerform(x + 17) should === (Some((x + 17).toString))
    }
  }
}

class CtrieSpecNaiveKCAS
  extends CtrieSpec
  with SpecNaiveKCAS

class CtrieSpecCASN
  extends CtrieSpec
  with SpecCASN

class CtrieSpecMCAS
  extends CtrieSpec
  with SpecMCAS
