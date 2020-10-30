/*
 * Copyright 2020 Daniel Urban and contributors listed in AUTHORS
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package dev.tauri.choam
package kcas

import java.util.concurrent.CountDownLatch

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalactic.TypeCheckedTripleEquals

final class IBRSpec
  extends AnyFlatSpec
  with Matchers
  with TypeCheckedTripleEquals {

  import IBRSpec.{ Descriptor, GC }

  "IBR" should "work" in {
    val gc = new GC
    val tc = gc.threadContext()
    tc.startOp()
    val ref = try {
      val d1 = tc.alloc(Descriptor("foo", _))
      val ref = Ref.mk(d1)
      val d2 = tc.alloc(Descriptor("bar", _))
      assert(tc.cas(ref, d1, d2))
      assert(!tc.cas(ref, d1, d2))
      assert(tc.read(ref) eq d2)
      tc.retire(d1)
      tc.retire(d2)
      ref
    } finally tc.endOp()
    tc.fullGc()
    assert(ref.unsafeTryRead().freed == 1)
  }

  it should "not free an object referenced from another thread" in {
    val gc = new GC
    val ref = Ref.mk[Descriptor](null)
    val latch1 = new CountDownLatch(1)
    val latch2 = new CountDownLatch(1)
    @volatile var error = false
    // bg thread:
    val t = new Thread(() => {
      val tc = gc.threadContext()
      tc.startOp()
      try {
        val d1 = tc.read(ref)
        latch1.countDown()
        latch2.await()
        // we're still using `d1` ...
        if (d1.freed > 0) {
          error = true
        }
      } finally tc.endOp()
    })
    // main thread:
    val tc = gc.threadContext()
    tc.startOp()
    val d1 = try {
      val d1 = tc.alloc(Descriptor("x", _))
      assert(tc.cas(ref, null, d1))
      t.start()
      latch1.await()
      // now `t` is still using `d1`, so it mustn't be freed, even if we retire it:
      tc.write(ref, null)
      tc.retire(d1)
      d1
    } finally tc.endOp()
    tc.fullGc()
    // make `t` end its op:
    latch2.countDown()
    t.join()
    assert(!error)
    // now it can be freed:
    tc.fullGc()
    assert(d1.freed == 1)
  }

  it should "reuse objects form the freelist" in {
    val gc = new GC
    val tc = gc.threadContext()
    tc.startOp()
    val d = try {
      val d = tc.alloc(Descriptor("x", _))
      tc.retire(d)
      d
    } finally tc.endOp()
    tc.fullGc()
    assert(d.freed == 1)
    tc.startOp()
    try {
      val d2 = tc.alloc(Descriptor("y", _))
      assert(d2 eq d)
      tc.retire(d2)
    } finally tc.endOp()
    tc.fullGc()
    assert(d.freed == 2)
  }

  "The epoch" should "be incremented after a few allocations" in {
    val gc = new GC
    val tc = gc.threadContext()
    val startEpoch = gc.epochNumber
    val descs = for (i <- 1 until IBR.epochFreq) yield {
      tc.startOp()
      try {
        tc.alloc(Descriptor(i.toString, _))
      } finally tc.endOp()
    }
    // the next allocation triggers the new epoch:
    tc.startOp()
    try {
      tc.alloc(Descriptor("new epoch", _))
    } finally tc.endOp()
    val newEpoch = gc.epochNumber
    assert(newEpoch === (startEpoch + 1))
    for (desc <- descs) {
      tc.startOp()
      try {
        tc.retire(desc)
      } finally tc.endOp()
    }
    for (desc <- descs) {
      assert(desc.birthEpoch.get() == startEpoch)
      assert(desc.retireEpoch.get() === newEpoch)
    }
  }

  "A reclamation" should "run after a few retirements" in {
    val gc = new GC
    val tc = gc.threadContext()
    val ref = Ref.mk(nullOf[Descriptor])
    val seen = new java.util.IdentityHashMap[Descriptor, Unit]
    @tailrec
    def go(cnt: Int): Int = {
      val done = tc.op {
        val d = tc.alloc(Descriptor(cnt.toString(), _))
        if (seen.containsKey(d)) {
          // a descriptor was freed and reused
          assert(d.freed == 1)
          // we're done
          true
        } else {
          seen.put(d, ())
          val prev = tc.read(ref)
          assert(tc.cas(ref, prev, d))
          if (prev ne null) tc.retire(prev)
          // continue:
          false
        }
      }
      if (done) cnt
      else go(cnt + 1)
    }
    val cnt = go(0)
    assert(cnt === IBR.emptyFreq)
  }

  "ThreadContext" should "be collected by the JVM GC if a thread terminates" in {
    val gc = new GC
    val tc = gc.threadContext()
    val firstEpoch = gc.epochNumber
    val ref = Ref.mk(nullOf[Descriptor])
    val d = tc.op {
      val d = tc.alloc(Descriptor("x", _))
      tc.write(ref, d)
      d
    }
    @volatile var error: Throwable = null
    val t = new Thread(() => {
      try {
        val tc = gc.threadContext()
        tc.startOp()
        val d = tc.read(ref)
        assert(d.freed === 0)
        // now the thread exits while still "using" the
        // descriptor, because it doesn't call `endOp`
        assert(tc.snapshotReservation.lower === firstEpoch)
        assert(tc.snapshotReservation.upper === firstEpoch)
        d.foobar()
      } catch {
        case ex: Throwable =>
         error = ex
         throw ex
      }
    })
    t.start()
    t.join()
    assert(!t.isAlive())
    assert(error eq null, s"error: ${error}")
    while (gc.snapshotReservations(t.getId()).get() ne null) {
      System.gc()
    }
    // now the `ThreadContext` have been collected by the JVM GC
    tc.op {
      tc.cas(ref, d, null)
      tc.retire(d)
    }
    assert(d.retireEpoch.get() === firstEpoch)
    tc.fullGc() // this should collect `d`
    assert(d.freed === 1)
  }
}

final object IBRSpec {

  final case class Descriptor(dummy: String, epoch: Long)
    extends IBR.Managed[Descriptor](epoch) {

    var freed = 0

    final override def free(): Unit = {
      this.freed += 1
    }

    final def foobar(): Unit = ()
  }

  final class GC extends IBR[Descriptor](zeroEpoch = 0L) {
    protected[kcas] override def dynamicTest[A](a: A): Boolean =
      a.isInstanceOf[Descriptor]
  }

  implicit final class ThreadContextSyntax(private val self: IBR.ThreadContext[_]) extends AnyVal {
    def op[A](body: => A): A = {
      self.startOp()
      try { body } finally { self.endOp() }
    }
  }
}
