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

import java.lang.Math
import java.lang.ref.{ WeakReference => WeakRef }
import java.lang.invoke.VarHandle
import java.util.concurrent.atomic.{ AtomicLong, AtomicReference, AtomicReferenceFieldUpdater }
import java.util.concurrent.ConcurrentSkipListMap

import scala.annotation.tailrec

/**
 * Interval-based memory reclamation (TagIBR-TPA), based on
 * the paper by Haosen Wen, Joseph Izraelevitz, Wentao Cai,
 * H. Alan Beadle, and Michael L. Scott, section 3.2 ("Using a
 * Type Preserving Allocator").
 *
 * @see https://www.cs.rochester.edu/u/scott/papers/2018_PPoPP_IBR.pdf
 *
 * @param `zeroEpoch` is the value of the very first epoch.
*/
private[kcas] abstract class IBR[T, M <: IBRManaged[T, M]](zeroEpoch: Long) {

  /** @return `true` iff `a` is really an `M` */
  protected[IBR] def dynamicTest[A](a: A): Boolean

  /** @return a newly allocated object */
  protected[IBR] def allocateNew(): M

  protected[IBR] def newThreadContext(): T

  /** Current epoch number, read/written by any thread */
  private[IBR] val epoch =
    new AtomicLong(zeroEpoch) // TODO: check if 64 bits is enough (overflow)

  /** For testing */
  private[kcas] def epochNumber: Long =
    this.epoch.get()

  /**
   * Reservations of all the (active) threads
   *
   * Threads hold a strong reference to their
   * `ThreadContext` in a thread local. Thus,
   * we only need a weakref here. If a thread
   * dies, its thread locals are cleared, so
   * the context can be GC'd (by the JVM). Empty
   * weakrefs are removed in `ThreadContext#empty`.
   *
   * Removing a dead thread's reservation will not
   * affect safety, because a dead thread will never
   * continue its current op (if any).
   */
  private[IBR] val reservations =
    new ConcurrentSkipListMap[Long, WeakRef[T]]

  /** For testing */
  private[kcas] def snapshotReservations: Map[Long, WeakRef[T]] = {
    @tailrec
    def go(
      it: java.util.Iterator[java.util.Map.Entry[Long, WeakRef[T]]],
      acc: Map[Long, WeakRef[T]]
    ): Map[Long, WeakRef[T]] = {
      if (it.hasNext()) {
        val n = it.next()
        go(it, acc.updated(n.getKey(), n.getValue()))
      } else {
        acc
      }
    }
    go(this.reservations.entrySet().iterator(), Map.empty)
  }

  /** Holds the context for each (active) thread */
  private[this] val threadContextKey =
    new ThreadLocal[T]()

  /** Gets of creates the context for the current thread */
  def threadContext(): T = {
    threadContextKey.get() match {
      case null =>
        val tc = this.newThreadContext()
        threadContextKey.set(tc)
        this.reservations.put(
          Thread.currentThread().getId(),
          new WeakRef(tc)
        )
        tc
      case tc =>
        tc
    }
  }
}

private[kcas] final object IBR {

  private[kcas] final val epochFreq = 128 // TODO

  private[kcas] final val emptyFreq = 256 // TODO

  assert(emptyFreq > epochFreq) // FIXME

  private[kcas] final val maxFreeListSize = 64 // TODO

  /** For testing */
  private[kcas] final case class SnapshotReservation(
    lower: Long,
    upper: Long
  )

  /**
   * Thread-local context of a thread
   *
   * Note: some fields can be accessed by other threads!
   */
  abstract class ThreadContext[T <: ThreadContext[T, M], M <: IBRManaged[T, M]](
    global: IBR[T, M]
  ) { this: T =>

    /**
     * Allocation counter for incrementing the epoch and running reclamation
     *
     * Overflow doesn't really matter, we only use it modulo `epochFreq` or `emptyFreq`.
     */
    private[this] var counter: Int =
      0

    /**
     * Intrusive linked list of retired (but not freed) objects
     *
     * It contains `retiredCount` items.
     */
    private[this] var retired: M =
      nullOf[M]

    /** Current size of the `retired` list */
    private[this] var retiredCount: Long =
      0L

    // TODO: Maybe add a failsafe: if `retiredCount`
    // TODO: is abnormally big, release some objects
    // TODO: without `free`-ing them. Since we'll use
    // TODO: IBR to remove EMCAS descriptors, this
    // TODO: shouldn't cause correctness problems, and
    // TODO: would avoid leaking a lot of memory if
    // TOOD: something goes terribly wrong.

    /**
     * Intrusive linked list of freed (reusable) objects
     *
     * It contains `freeListSize` items (at most `maxFreeListSize`).
     */
    private[this] var freeList: M =
      nullOf[M]

    private[this] var freeListSize: Long =
      0L

    /**
     * Epoch interval reserved by the thread.
     *
     * Note: read by other threads when reclaiming memory!
     */
    private[IBR] final val reservation: IBRReservation =
      new IBRReservation(Long.MaxValue)

    /** For testing */
    private[kcas] def snapshotReservation: SnapshotReservation = {
      SnapshotReservation(
        lower = this.reservation.getLower(),
        upper = this.reservation.getUpper()
      )
    }

    /** For testing */
    private[kcas] def globalContext: IBR[T, M] =
      global

    /** For testing */
    private[kcas] def op[A](body: => A): A = {
      this.startOp()
      try { body } finally { this.endOp() }
    }

    final def alloc(): M = {
      this.counter += 1
      val epoch = if ((this.counter % epochFreq) == 0) {
        this.global.epoch.incrementAndGet()
      } else {
        this.global.epoch.get()
      }
      val elem = if (this.freeList ne null) {
        this.freeListSize -= 1
        val elem = this.freeList
        this.freeList = elem.getNext()
        elem.setNext(nullOf[M])
        elem
      } else {
        global.allocateNew()
      }
      elem.setBirthEpoch(epoch)
      elem.allocate(this)
      elem
    }

    final def retire(a: M): Unit = {
      this.retiredCount += 1L
      a.setNext(this.retired)
      this.retired = a
      a.setRetireEpoch(this.global.epoch.get())
      a.retire(this)
      if ((this.counter % emptyFreq) == 0) {
        this.empty()
      }
    }

    final def startOp(): Unit = {
      reserve(this.global.epoch.get())
    }

    final def endOp(): Unit = {
      reserve(Long.MaxValue)
    }

    @tailrec
    final def readArfu[A](arfu: AtomicReferenceFieldUpdater[M, A], obj: M): A = {
      val a: A = arfu.get(obj)
      if (tryAdjustReservation(a)) a
      else readArfu(arfu, obj)
    }

    @tailrec
    final def readVh[A](vh: VarHandle, obj: M): A = {
      val a: A = vh.getVolatile(obj)
      if (tryAdjustReservation(a)) a
      else readVh(vh, obj) // retry
    }

    @tailrec
    final def read[A](ref: AtomicReference[A]): A = {
      val a: A = ref.get() // getAcquire might be enough(?)
      if (tryAdjustReservation(a)) a
      else read(ref) // retry
    }

    private[kcas] final def tryAdjustReservation[A](a: A): Boolean = {
      if (this.global.dynamicTest(a)) {
        val m: M = a.asInstanceOf[M]
        val res: IBRReservation = this.reservation
        val currUpper = res.getUpper()
        res.setUpper(Math.max(currUpper, m.getBirthEpoch()))
        // `m` might've been retired before we ajusted
        // our reservation, so we have to recheck the
        // birth epoch:
        if (res.getUpper() >= m.getBirthEpoch()) {
          // ok, we're done
          true
        } else {
          // `m` was probably retired, freed and reused;
          // its birth epoch is therefore greater; we
          // need to re-read the ref, and try again
          false
        }
      } else {
        true // we're done
      }
    }

    final def writeArfu[A](arfu: AtomicReferenceFieldUpdater[M, A], obj: M, a: A): Unit = {
      arfu.set(obj, a)
    }

    final def writeVh[A](vh: VarHandle, obj: M, a: A): Unit = {
      vh.setVolatile(obj, a)
    }

    final def write[A](ref: AtomicReference[A], nv: A): Unit = {
      ref.set(nv)
    }

    final def casArfu[A](arfu: AtomicReferenceFieldUpdater[M, A], obj: M, ov: A, nv: A): Boolean = {
      arfu.compareAndSet(obj, ov, nv)
    }

    final def casVh[A](vh: VarHandle, obj: M, ov: A, nv: A): Boolean = {
      vh.compareAndSet(obj, ov, nv)
    }

    final def cas[A](ref: AtomicReference[A], ov: A, nv: A): Boolean = {
      ref.compareAndSet(ov, nv)
    }

    /** For testing */
    private[kcas] def isDuringOp(): Boolean = {
      (this.reservation.getLower() != Long.MaxValue) || (
        this.reservation.getUpper() != Long.MaxValue
      )
    }

    /** For testing */
    private[kcas] def getRetiredCount(): Long = {
      this.retiredCount
    }

    /** For testing */
    private[kcas] def forceGc(): Unit = {
      assert(this.isDuringOp())
      this.empty()
    }

    /** For testing */
    private[kcas] def forceNextEpoch(): Unit = {
      assert(!this.isDuringOp())
      this.global.epoch.getAndIncrement()
      ()
    }

    private[kcas] def fullGc(): Unit = {
      forceNextEpoch()
      startOp()
      try forceGc()
      finally endOp()
    }

    private def reserve(epoch: Long): Unit = {
      this.reservation.setLower(epoch)
      this.reservation.setUpper(epoch)
    }

    private def empty(): Unit = {
      val reservations = this.global.reservations.values()
      @tailrec
      def go(curr: M, prev: M): Unit = {
        if (curr ne null) {
          val currNext = curr.getNext() // save next, because `free` may clear it
          var newPrev = curr // `prev` for the next iteration
          if (!isConflict(curr, reservations.iterator())) {
            // remove `curr` from the `retired` list:
            this.retiredCount -= 1L
            if (prev ne null) {
              // delete an internal item:
              prev.setNext(curr.getNext())
            } else {
              // delete the head:
              this.retired = curr.getNext()
            }
            free(curr) // actually free `curr`
            newPrev = prev // since we removed `curr` form the list
          }
          go(currNext, newPrev)
        } // else: end of `retired` list
      }
      @tailrec
      def isConflict(block: M, it: java.util.Iterator[WeakRef[T]]): Boolean = {
        if (it.hasNext()) {
          val wr = it.next()
          wr.get() match {
            case null =>
              it.remove()
              isConflict(block, it) // continue
            case tc =>
              val conflict = (
                (block.getBirthEpoch() <=  tc.reservation.getUpper()) &&
                (block.getRetireEpoch() >= tc.reservation.getLower())
              )
              if (conflict) true
              else isConflict(block, it) // continue
          }
        } else {
          false
        }
      }

      go(this.retired, prev = nullOf[M])
    }

    private def free(block: M): Unit = {
      block.free(this)
      block.setBirthEpoch(Long.MinValue) // TODO: not strictly necessary
      block.setRetireEpoch(Long.MaxValue) // TODO: not strictly necessary
      if (this.freeListSize < maxFreeListSize) {
        this.freeListSize += 1
        block.setNext(this.freeList)
        this.freeList = block
      } else {
        block.setNext(nullOf[M])
      }
    }
  }
}
