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
    // TODO: thread safety -- only read after reading from a Ref(?)
    val words: ArrayList[WordDescriptor[_]],
    var isSorted: Boolean
  ) extends self.Desc with self.Snap {

    val status =
      new AtomicReference[StatusType](Active)

    override def withCAS[A](ref: Ref[A], ov: A, nv: A): Desc = {
      this.words.add(new WordDescriptor[A](ref, ov, nv, this))
      if (this.words.size() > 1) {
        this.isSorted = false
      } // else: empty and 1-element is always sorted
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
      val r = new MCASDescriptor(newArr, this.isSorted)
      copy(this.words, newArr, r, 0, this.words.size())
      r
    }

    def sort(): Unit = {
      if (!this.isSorted) {
        this.words.sort(WordDescriptor.comparator)
        this.isSorted = true
      }
    }

    override def tryPerform(): Boolean = {
      MCAS(this)
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

  // TODO: verify that this is zero-cost
  final object DescOr {

    type _Base
    trait _Tag extends Any
    type Type[A] <: _Base with _Tag

    def wrap[A](a: A): Type[A] =
      a.asInstanceOf[Type[A]]

    def desc[A](d: WordDescriptor[A]): Type[A] =
      d.asInstanceOf[Type[A]]

    def isData[A](t: Type[A]): Boolean =
      !isDescriptor(t)

    def isDescriptor[A](t: Type[A]): Boolean =
      t.isInstanceOf[WordDescriptor[_]]

    def asData[A](t: Type[A]): A =
      t.asInstanceOf[A]

    def asDescriptor[A](t: Type[A]): WordDescriptor[A] =
      t.asInstanceOf[WordDescriptor[A]]
  }

  private def rawRead[A](ref: Ref[A]): DescOr.Type[A] =
    DescOr.wrap(ref.unsafeTryRead())

  // Listing 2 in the paper:

  @tailrec
  def readInternal[A](ref: Ref[A], self: MCASDescriptor): (DescOr.Type[A], A) = {
    val o = rawRead(ref)
    if (DescOr.isDescriptor(o)) {
      val wd: WordDescriptor[A] = DescOr.asDescriptor(o)
      val parent = wd.parent
      val parentStatus = parent.status.get()
      if ((parent ne self) && (parentStatus eq Active)) {
        MCAS(parent)
        readInternal(ref, self) // retry
      } else if (parentStatus eq Successful) {
        (o, wd.nv)
      } else {
        (o, wd.ov)
      }
    } else {
      (o, DescOr.asData(o))
    }
  }

  // Listing 3 in the paper:

  private[choam] override def tryReadOne[A](ref: Ref[A]): A = {
    readInternal(ref, null)._2
  }

  def MCAS(desc: MCASDescriptor): Boolean = {
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
        true // TODO: true, but we should break from `go`
      } else {
        if (!wordDesc.address.unsafeTryPerformCas(DescOr.asData(content), wordDesc.asInstanceOf[A])) {
          tryWord(wordDesc) // retry
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
    desc.sort() // NB: this is either a NOP, or it's BEFORE `desc` is visible to other threads
    val success = go(desc.words.iterator)
    if (desc.status.compareAndSet(Active, if (success) Successful else Failed)) {
      // TODO: we finalized the descriptor, mark it for reclamation
      success
    } else {
      // someone else finalized the descriptor, must read its status:
      desc.status.get() eq Successful
    }
  }

  private[choam] override def start(): Desc = {
    new MCASDescriptor(
      new ArrayList(MCASDescriptor.minArraySize),
      isSorted = true // empty is always sorted
    )
  }
}
