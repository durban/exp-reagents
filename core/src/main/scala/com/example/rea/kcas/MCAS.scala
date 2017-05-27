package com.example.rea
package kcas

import java.lang.ThreadLocal
import java.util.concurrent.atomic.AtomicInteger

/**
 * An optimized version of `CASN`.
 */
private[kcas] object MCAS extends KCAS {

  override def start(): this.Desc = {
    val desc = TlSt.get().loanDescriptor()
    desc.start()
    desc
  }

  override def tryReadOne[A](ref: Ref[A]): A =
    read(ref)

  private sealed trait RDCSSResult
  private final case object AcquireSuccess extends RDCSSResult
  private final case object AcquireFailure extends RDCSSResult
  private final case class OtherDescriptor(d: MCASDesc) extends RDCSSResult

  @tailrec
  private def read[A](ref: Ref[A]): A = {
    val r = RDCSSRead(ref)
    r match {
      case d: MCASDesc =>
        // help the other op:
        d.incr()
        if (equ(ref.unsafeTryRead(), d.as[A])) {
          d.perform()
        } else {
          d.decr()
        }
        // and retry:
        read(ref)
      case _ =>
        // ok, we found it:
        r
    }
  }

  private def RDCSSRead[A](ref: Ref[A]): A = {
    @tailrec
    def go(): A = {
      val r: A = ref.unsafeTryRead()
      (r : Any) match {
        case e: MCASEntry =>
          // try to help the other thread, then retry:
          val desc = e.desc
          if (desc ne null) {
            desc.incr()
            if (equ(ref.unsafeTryRead(), r) && (e.desc eq desc)) {
              try {
                RDCSSComp(desc.status, Undecided, e, desc)
              } finally {
                desc.decr()
              }
            } else {
              desc.decr()
            }
          } // else: was released, we can retry
          go()
        case _ =>
          // ok:
          r
      }
    }

    go()
  }

  private def RDCSSComp[A](
    status: Ref[MCASStatus],
    expSt: MCASStatus,
    entry: MCASEntry,
    nv: MCASDesc,
  ): Unit = {
    val s: MCASStatus = status.unsafeTryRead()
    if (equ(s, expSt)) {
      CAS1fromEntry[entry.A](entry.ref, entry, nv.as[entry.A])
    } else {
      CAS1fromEntry(entry.ref, entry, entry.ov)
    }
  }

  private def CAS1toEntry[A](ref: Ref[A], ov: A, nv: MCASEntry): Boolean =
    ref.unsafeTryPerformCas(ov, nv.as[A])

  private def CAS1fromEntry[A](ref: Ref[A], ov: MCASEntry, nv: A): Boolean =
    ref.unsafeTryPerformCas(ov.as[A], nv)

  private def CAS1fromDesc[A](ref: Ref[A], ov: MCASDesc, nv: A): Boolean =
    ref.unsafeTryPerformCas(ov.as[A], nv)

  private sealed trait MCASStatus
  private final case object Undecided extends MCASStatus
  private sealed trait Decided extends MCASStatus
  private final case object Failed extends Decided
  private final case object Succeeded extends Decided

  private final class MCASDesc extends this.Desc {

    private[this] val refcount =
      new AtomicInteger(1) // LSB is a claim flag

    private[MCAS] val status: Ref[MCASStatus] =
      Ref.mk(Undecided)

    var next: MCASDesc =
      _

    private[this] var head: MCASEntry =
      _

    def rawRefCnt(): Int =
      refcount.get()

    def incr(): Unit =
      refcount.addAndGet(2)

    def decr(): Unit = {
      if (decrementAndTestAndSet()) {
        val tlst = TlSt.get()
        // release entries:
        while (head ne null) {
          val e = head
          head = e.next
          tlst.releaseEntry(e)
        }
        // release descriptor:
        tlst.releaseDescriptor(this)
      }
    }

    /**
     * @return true iff the (logical) refcount reached 0
     */
    @tailrec
    private def decrementAndTestAndSet(): Boolean = {
      val ov: Int = refcount.get()
      val (nv, rel) = if (ov == 2) {
        (1, true)
      } else {
        (ov - 2, false)
      }
      if (refcount.compareAndSet(ov, nv)) {
        rel
      } else {
        decrementAndTestAndSet()
      }
    }

    @tailrec
    def clearLowestBit(): Unit = {
      val ov: Int = refcount.get()
      assert((ov % 2) == 1, "lowest bit is not set")
      val nv = ov - 1
      if (!refcount.compareAndSet(ov, nv)) {
        clearLowestBit()
      }
    }

    def start(): Unit = {
      assert(head eq null, "head of new descriptor is not null")
      status.unsafeSet(Undecided)
    }

    override def withCAS[A](ref: Ref[A], ov: A, nv: A): this.type = {
      val entry = TlSt.get().loanEntry[A]()
      entry.ref = ref
      entry.ov = ov
      entry.nv = nv
      entry.desc = this
      entry.next = head
      head = entry
      this
    }

    override def tryPerform(): Boolean = {
      sort()
      perform()
    }

    private def sort(): Unit = {
      // TODO!!!
    }

    // Can be called from other threads!
    private[MCAS] def perform(): Boolean = {
      try {
        // Phase 1 starts from UNDECIDED:
        if (status.unsafeTryRead() eq Undecided) {
          @tailrec
          def phase1(entry: MCASEntry): Decided = {
            if (entry eq null) {
              Succeeded
            } else {
              val res = RDCSStoDesc(status, Undecided, entry, this)
              res match {
                case OtherDescriptor(that) =>
                  if (this ne that) {
                    // help the other op:
                    that.incr()
                    if (equ(entry.ref.unsafeTryRead(), that.as[entry.A])) {
                      that.perform()
                    } else {
                      that.decr()
                    }
                    // and retry from this entry:
                    phase1(entry)
                  } else {
                    // somebody helped us, continue:
                    phase1(entry.next)
                  }
                case AcquireFailure =>
                  // other op succeeded:
                  Failed
                case AcquireSuccess =>
                  // continue:
                  phase1(entry.next)
              }
            }
          }

          val decision: Decided = phase1(head)
          status.unsafeTryPerformCas(Undecided, decision)
        }

        // Phase 2 (now status is either FAILED or SUCCEEDED):
        val failOrSucc = status.unsafeTryRead()
        assert((failOrSucc eq Failed) || (failOrSucc eq Succeeded), s"status is not decided but ${failOrSucc}")
        val succeeded = (failOrSucc eq Succeeded)

        @tailrec
        def phase2(entry: MCASEntry): Boolean = {
          if (entry eq null) {
            // we're done:
            succeeded
          } else {
            CAS1fromDesc(entry.ref, this, if (succeeded) entry.nv else entry.ov)
            phase2(entry.next)
          }
        }

        phase2(head)
      } finally {
        decr()
      }
    }

    private def RDCSStoDesc[A](
      status: Ref[MCASStatus],
      expSt: MCASStatus,
      entry: MCASEntry,
      nv: MCASDesc,
    ): RDCSSResult = {
      @tailrec
      def acquire(): RDCSSResult = {
        if (CAS1toEntry(entry.ref, entry.ov, entry)) {
          // ok, we succeeded:
          AcquireSuccess
        } else {
          // we failed ...
          entry.ref.unsafeTryRead() match {
            case e: MCASEntry =>
              // other op underway, let's help:
              val desc = e.desc
              if (desc ne null) {
                desc.incr()
                if (equ(entry.ref.unsafeTryRead(), e.as[entry.A]) && (e.desc eq desc)) {
                  try {
                    RDCSSComp(status, expSt, e, nv)
                  } finally {
                    desc.decr()
                  }
                } else {
                  desc.decr()
                }
              } // else: was released in the meantime
              // retry ours:
              acquire()
            case d: MCASDesc =>
              // TODO: avoid allocation here
              OtherDescriptor(d)
            case _ =>
              // probably other op completed before us:
              AcquireFailure
          }
        }
      }

      val res = acquire()
      res match {
        case AcquireSuccess =>
          RDCSSComp(status, expSt, entry, nv)
        case _ =>
          // `acquire` failed, no need
          // to call `RDCSSComp`, it
          // would fail for sure
          ()
      }

      res
    }

    private[MCAS] def as[A]: A =
      this.asInstanceOf[A]
  }

  private final class MCASEntry() {

    type A

    var ref: Ref[A] = _
    var ov: A = _
    var nv: A = _
    var desc: MCASDesc = _
    var next: MCASEntry = _

    private[MCAS] def as[X]: X =
      this.asInstanceOf[X]
  }

  private final class TlSt {

    private[this] var freeEntries: MCASEntry =
      _

    private[this] var freeDescriptors: MCASDesc =
      _

    def loanEntry[A0](): MCASEntry { type A = A0 } = {
      val res: MCASEntry = freeEntries
      if (res eq null) {
        (new MCASEntry).asInstanceOf[MCASEntry { type A = A0 }]
      } else {
        freeEntries = res.next
        res.asInstanceOf[MCASEntry { type A = A0 }]
      }
    }

    def releaseEntry(e: MCASEntry): Unit = {
      e.ref = null
      e.ov = nullOf[e.A]
      e.nv = nullOf[e.A]
      e.desc = null
      e.next = freeEntries
      //XXX: freeEntries = e
    }

    def loanDescriptor(): MCASDesc = {
      val nxt = freeDescriptors
      val res = if (nxt eq null) {
        new MCASDesc
      } else {
        freeDescriptors = nxt.next
        nxt
      }
      res.incr()
      res.clearLowestBit()
      res
    }

    def releaseDescriptor(d: MCASDesc) = {
      d.next = freeDescriptors
      //XXX: freeDescriptors = d
    }
  }

  private final object TlSt {

    private[this] val inst =
      new ThreadLocal[TlSt]()

    def get(): TlSt = {
      val res = inst.get()
      if (res eq null) {
        val ntl = new TlSt
        inst.set(ntl)
        ntl
      } else {
        res
      }
    }
  }
}
