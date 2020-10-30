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
import java.util.concurrent.atomic.AtomicLong

import scala.annotation.tailrec
import scala.collection.concurrent.TrieMap

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
private[kcas] abstract class IBR[M <: IBR.Managed[M]](zeroEpoch: Long) {

  /** @return `true` iff `a` is really an `M` */
  protected[IBR] def dynamicTest[A](a: A): Boolean

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
    new TrieMap[Long, WeakRef[IBR.ThreadContext[M]]]

  /** For testing */
  private[kcas] def snapshotReservations: Map[Long, WeakRef[IBR.ThreadContext[M]]] =
    this.reservations.readOnlySnapshot().toMap

  /** Holds the context for each (active) thread */
  private[this] val threadContextKey =
    new ThreadLocal[IBR.ThreadContext[M]]()

  /** Gets of creates the context for the current thread */
  def threadContext(): IBR.ThreadContext[M] = {
    threadContextKey.get() match {
      case null =>
        val tc = new IBR.ThreadContext(this)
        threadContextKey.set(tc)
        reservations.put(
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

  /**
   * Base class for objects managed by IBR
   *
   * @param `birthEp` the initial birth epoch of the object.
   */
  abstract class Managed[M <: IBR.Managed[M]] { this: M =>

    /** Intrusive linked list (free/retired list) */
    private[IBR] var next: M =
      _

    // TODO: verify that we really don't need born_before from the paper

    // TODO: could we have these as simple `var`s?
    private[kcas] val birthEpoch =
      new AtomicLong(Long.MinValue)

    private[kcas] val retireEpoch =
      new AtomicLong(Long.MaxValue)

    /** Hook for subclasses for performing cleanup */
    protected[IBR] def free(tc: ThreadContext[M]): Unit

    /** Hook for subclasses for performing initialization */
    protected[IBR] def allocate(tc: ThreadContext[M]): Unit
  }

  /** The epoch interval reserved by a thread */
  private final class Reservation(initial: Long) {
    val lower: AtomicLong = new AtomicLong(initial)
    val upper: AtomicLong = new AtomicLong(initial)
  }

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
  final class ThreadContext[M <: IBR.Managed[M]](global: IBR[M]) {

    /**
     * Allocation counter for incrementing the epoch and running reclamation
     *
     * Overflow doesn't really matter, we only use it modulo `epochFreq` or `emptyFreq`.
     */
    private[this] var counter: Int =
      0

    /** Intrusive linked list of retired (but not freed) objects */
    private[this] var retired: M =
      nullOf[M]

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
    private[IBR] val reservation: Reservation =
      new Reservation(Long.MaxValue)

    /** For testing */
    private[kcas] def snapshotReservation: SnapshotReservation = {
      SnapshotReservation(
        lower = this.reservation.lower.get(),
        upper = this.reservation.upper.get()
      )
    }

    /** For testing */
    private[kcas] def globalContext: IBR[M] =
      global

    /** For testing */
    private[kcas] def op[A](body: => A): A = {
      this.startOp()
      try { body } finally { this.endOp() }
    }

    // FIXME: `makeNew` being here is not really useful,
    // FIXME: since it might not be used, so we can't count on it.
    def alloc(makeNew: Function0[M]): M = {
      this.counter += 1
      val epoch = if ((this.counter % epochFreq) == 0) {
        this.global.epoch.incrementAndGet()
      } else {
        this.global.epoch.get()
      }
      val elem = if (this.freeList ne null) {
        this.freeListSize -= 1
        val elem = this.freeList
        this.freeList = elem.next
        elem
      } else {
        makeNew()
      }
      elem.birthEpoch.set(epoch)
      elem.allocate(this)
      elem
    }

    def retire(a: M): Unit = {
      a.next = this.retired
      this.retired = a
      a.retireEpoch.set(this.global.epoch.get())
      if ((this.counter % emptyFreq) == 0) {
        this.empty()
      }
    }

    def startOp(): Unit = {
      reserve(this.global.epoch.get())
    }

    def endOp(): Unit = {
      reserve(Long.MaxValue)
    }

    @tailrec
    def read[A](ref: Ref[A]): A = {
      val a: A = ref.unsafeTryRead()
      if (this.global.dynamicTest(a)) {
        val m: M = a.asInstanceOf[M]
        val upper = this.reservation.upper.get()
        this.reservation.upper.set(Math.max(upper, m.birthEpoch.get()))
        if (this.reservation.upper.get() >= m.birthEpoch.get()) {
          a // ok, we're done
        } else {
          read(ref) // retry
        }
      } else {
        a
      }

    }

    def write[A](ref: Ref[A], nv: A): Unit = {
      ref.unsafeSet(nv)
    }

    def cas[A](ref: Ref[A], ov: A, nv: A): Boolean = {
      ref.unsafeTryPerformCas(ov, nv)
    }

    /** For testing */
    private[kcas] def forceGc(): Unit = {
      assert(this.reservation.lower.get() < Long.MaxValue)
      assert(this.reservation.upper.get() < Long.MaxValue)
      this.empty()
    }

    /** For testing */
    private[kcas] def forceNextEpoch(): Unit = {
      assert(this.reservation.lower.get() == Long.MaxValue)
      assert(this.reservation.upper.get() == Long.MaxValue)
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
      this.reservation.lower.set(epoch)
      this.reservation.upper.set(epoch)
    }

    private def empty(): Unit = {
      val reservations = this.global.reservations
      @tailrec
      def go(curr: M, prev: M, deleteHead: Boolean): Boolean = {
        if (curr ne null) {
          val del = if (!isConflict(curr, reservations.iterator)) {
            free(curr)
            if (prev ne null) {
              prev.next = curr.next
              deleteHead
            } else {
              true
            }
          } else {
            deleteHead
          }
          go(curr.next, curr, del)
        } else {
          deleteHead
        }
      }
      @tailrec
      def isConflict(block: M, it: Iterator[(Long, WeakRef[ThreadContext[M]])]): Boolean = {
        if (it.hasNext) {
          val (tid, wr) = it.next()
          wr.get() match {
            case null =>
              reservations.remove(tid, wr)
              isConflict(block, it)
            case tc =>
              val conflict = (
                (block.birthEpoch.get() <=  tc.reservation.upper.get()) &&
                (block.retireEpoch.get() >= tc.reservation.lower.get())
              )
              if (conflict) true
              else isConflict(block, it)
          }
        } else {
          false
        }
      }

      val delHead = go(this.retired, prev = nullOf[M], deleteHead = false)
      if (delHead) {
        this.retired = this.retired.next
      }
    }

    private def free(block: M): Unit = {
      block.free(this)
      block.birthEpoch.set(Long.MinValue) // TODO: not strictly necessary
      block.retireEpoch.set(Long.MaxValue) // TODO: not strictly necessary
      if (this.freeListSize < maxFreeListSize) {
        this.freeListSize += 1
        block.next = this.freeList
        this.freeList = block
      } // else: JVM GC will collect
    }
  }
}
