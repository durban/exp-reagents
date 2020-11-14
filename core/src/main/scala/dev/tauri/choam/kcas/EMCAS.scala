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

import java.util.concurrent.atomic.AtomicReference
import java.util.{ ArrayList, Comparator }

import scala.annotation.tailrec

/**
 * Efficient Multi-word Compare and Swap:
 * https://arxiv.org/pdf/2008.02527.pdf
 */
private[kcas] object EMCAS extends KCAS { self =>

  // Listing 1 in the paper:

  final object MCASDescriptor {
    final val minArraySize = 8
  }

  final class MCASDescriptor(
    /**
     * Word descriptors
     *
     * Thread safety: we only read the list after reading the descriptor from a `Ref`;
     * we only mutate the list before writing the descriptor to a `Ref`.
     */
    val words: ArrayList[WordDescriptor[_]]
  ) extends self.Desc with self.Snap {

    val status =
      new AtomicReference[StatusType](Active)

    override def withCAS[A](ref: Ref[A], ov: A, nv: A): Desc = {
      this.words.add(new WordDescriptor[A](ref, ov, nv, this))
      this
    }

    override def snapshot(): Snap = {
      @tailrec
      def copy(
        from: ArrayList[WordDescriptor[_]],
        to: ArrayList[WordDescriptor[_]],
        newParent: MCASDescriptor,
        idx: Int,
        len: Int
      ): Unit = {
        if (idx < len) {
          to.add(from.get(idx).withParent(newParent))
          copy(from, to, newParent, idx + 1, len)
        }
      }
      val newArrCapacity = Math.max(this.words.size(), MCASDescriptor.minArraySize)
      val newArr = new ArrayList[WordDescriptor[_]](newArrCapacity)
      val r = new MCASDescriptor(newArr)
      copy(this.words, newArr, r, 0, this.words.size())
      r
    }

    def sort(): Unit = {
      this.words.sort(WordDescriptor.comparator)
    }

    override def tryPerform(): Boolean = {
      MCAS(this, helping = false)
    }

    override def cancel(): Unit =
      ()

    override def load(): Desc =
      this

    override def discard(): Unit =
      ()
  }

  final class WordDescriptor[A](
    val address: Ref[A],
    val ov: A,
    val nv: A,
    val parent: MCASDescriptor
  ) {
    def withParent(newParent: MCASDescriptor): WordDescriptor[A] =
      new WordDescriptor[A](this.address, this.ov, this.nv, newParent)
  }

  final object WordDescriptor {
    val comparator: Comparator[WordDescriptor[_]] = new Comparator[WordDescriptor[_]] {
      final override def compare(x: WordDescriptor[_], y: WordDescriptor[_]): Int = {
        // NB: `x ne y` is always true, because we create fresh descriptors in `withCAS`
        val res = Ref.globalCompare(x.address, y.address)
        if (res == 0) {
          assert(x.address eq y.address)
          KCAS.impossibleKCAS(x.address, x.ov, x.nv, y.ov, y.nv)
        } else {
          res
        }
      }
    }
  }

  sealed abstract class StatusType extends Product with Serializable
  final case object Active extends StatusType
  final case object Successful extends StatusType
  final case object Failed extends StatusType

  final object DescOr {

    type _Base
    trait _Tag extends Any
    type Type[A] <: _Base with _Tag

    @inline
    def wrap[A](a: A): Type[A] =
      a.asInstanceOf[Type[A]] // NOP

    @inline
    def desc[A](d: WordDescriptor[A]): Type[A] =
      d.asInstanceOf[Type[A]] // NOP

    def isData[A](t: Type[A]): Boolean =
      !isDescriptor(t) // instanceof, negate

    @inline
    def isDescriptor[A](t: Type[A]): Boolean =
      t.isInstanceOf[WordDescriptor[_]] // instanceof

    @inline
    def asData[A](t: Type[A]): A =
      t.asInstanceOf[A] // NOP

    @inline
    def asDescriptor[A](t: Type[A]): WordDescriptor[A] =
      t.asInstanceOf[WordDescriptor[A]] // checkcast
  }

  private def rawRead[A](ref: Ref[A]): DescOr.Type[A] =
    DescOr.wrap(ref.unsafeTryRead())

  // Listing 2 in the paper:

  // TODO: try to avoid allocating the return tuple
  @tailrec
  def readInternal[A](ref: Ref[A], self: MCASDescriptor): (DescOr.Type[A], A) = {
    val o = rawRead(ref)
    if (DescOr.isDescriptor(o)) {
      val wd: WordDescriptor[A] = DescOr.asDescriptor(o)
      val parentStatus = wd.parent.status.get()
      if ((wd.parent ne self) && (parentStatus eq Active)) { // `parent ne self` is to not "help" ourselves
        MCAS(wd.parent, helping = true) // help the other op
        readInternal(ref, self) // retry
      } else {
        if (parentStatus eq Successful) {
          (o, wd.nv)
        } else { // Failed OR ((parent eq self) AND !Successful)
          (o, wd.ov)
        }
      }
    } else {
      (o, DescOr.asData(o))
    }
  }

  // Listing 3 in the paper:

  private[choam] override def tryReadOne[A](ref: Ref[A]): A = {
    readInternal(ref, null)._2
  }

  /**
   * Performs an MCAS operation.
   *
   * @param desc: The main descriptor.
   * @param helping: Pass `true` when helping `desc` found in a `Ref`;
   *                 `false` when `desc` is a new descriptor.
   */
  def MCAS(desc: MCASDescriptor, helping: Boolean): Boolean = {
    @tailrec
    def tryWord[A](wordDesc: WordDescriptor[A]): Boolean = {
      val (content, value) = readInternal(wordDesc.address, desc)
      if (equ[Any](content, wordDesc)) {
        // already points to the right place
        true
      } else if (!equ(value, wordDesc.ov)) {
        // expected value is different
        false
      } else if (desc.status.get() ne Active) {
        // we have been finalized (by a helping thread), no reason to continue
        true // TODO: we should break from `go`
        // TODO: `true` is not necessarily correct, the helping thread could've finalized us to failed too
      } else {
        if (!wordDesc.address.unsafeTryPerformCas(DescOr.asData(content), wordDesc.asInstanceOf[A])) {
          tryWord(wordDesc) // retry this word
        } else {
          true
        }
      }
    }
    @tailrec
    def go(words: java.util.Iterator[WordDescriptor[_]]): Boolean = {
      if (words.hasNext) {
        val wordDesc = words.next()
        if (tryWord(wordDesc)) go(words)
        else false
      } else {
        true
      }
    }
    if (!helping) {
      // we're not helping, so `desc` is not yet visible to other threads
      desc.sort()
    } // else: the thread which published `desc` already sorted it
    val success = go(desc.words.iterator)
    if (desc.status.compareAndSet(Active, if (success) Successful else Failed)) {
      // TODO: We finalized the descriptor, mark it for reclamation:
      // TODO: retireForCleanup(desc)
      success
    } else {
      // someone else finalized the descriptor, must read its status:
      desc.status.get() eq Successful
    }
  }

  private[choam] override def start(): Desc = {
    new MCASDescriptor(new ArrayList(MCASDescriptor.minArraySize))
  }
}
