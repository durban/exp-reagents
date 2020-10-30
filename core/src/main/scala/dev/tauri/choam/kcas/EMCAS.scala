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

import scala.annotation.tailrec
import scala.collection.mutable.ArrayBuffer

/**
 * Efficient Multi-word Compare and Swap:
 * https://arxiv.org/pdf/2008.02527.pdf
 */
private[kcas] object EMCAS extends KCAS { self =>

  // Listing 1 in the paper:

  final class MCASDescriptor(
    val words: ArrayBuffer[WordDescriptor[_]]
  ) extends self.Desc with self.Snap {

    val status =
      new AtomicReference[StatusType](Active)

    override def withCAS[A](ref: Ref[A], ov: A, nv: A): Desc = {
      // TODO: thread safety -- only read after reading from a Ref(?)
      this.words.addOne(new WordDescriptor[A](ref, ov, nv, this))
      this
    }

    override def snapshot(): Snap = {
      // TODO: thread safety -- only read after reading from a Ref(?)
      new MCASDescriptor(ArrayBuffer.from(this.words))
    }

    override def tryPerform(): Boolean = {
      MCAS(this)
    }

    override def cancel(): Unit =
      () // TODO

    override def load(): Desc =
      this

    override def discard(): Unit =
      () // TODO
  }

  final class WordDescriptor[A](
    val address: Ref[A],
    val ov: A,
    val nv: A,
    val parent: MCASDescriptor
  )

  sealed abstract class StatusType extends Product with Serializable
  final case object Active extends StatusType
  final case object Successful extends StatusType
  final case object Failed extends StatusType

  // Listing 2 in the paper:

  @tailrec
  def readInternal[A](ref: Ref[A], self: MCASDescriptor): (A, A) = {
    val v = ref.unsafeTryRead()
    v match {
      case wd: WordDescriptor[A] =>
        val parent = wd.parent
        val parentStatus = parent.status.get()
        if ((parent ne self) && (parentStatus eq Active)) {
          MCAS(parent)
          readInternal(ref, self)
        } else if (parentStatus eq Successful) {
          (v, wd.nv)
        } else {
          (v, wd.ov)
        }
      case v =>
        (v, v)
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
        if (!wordDesc.address.unsafeTryPerformCas(content, wordDesc.asInstanceOf[A])) {
          tryWord(wordDesc) // retry
        } else {
          true
        }
      }
    }
    @tailrec
    def go(words: Iterator[WordDescriptor[_]]): Boolean = {
      if (words.hasNext) {
        val wordDesc = words.next()
        if (tryWord(wordDesc)) go(words)
        else false
      } else {
        true
      }
    }
    val success = go(desc.words.iterator) // TODO: sort descriptors!
    if (desc.status.compareAndSet(Active, if (success) Successful else Failed)) {
      // TODO: we finalized the descriptor, mark it for reclamation
      success
    } else {
      // someone else finalized the descriptor, must read its status:
      desc.status.get() eq Successful
    }
  }

  private[choam] override def start(): Desc = {
    new MCASDescriptor(ArrayBuffer.empty)
  }
}
