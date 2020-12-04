/*
 * SPDX-License-Identifier: Apache-2.0
 * Copyright 2016-2020 Daniel Urban and contributors listed in NOTICE.txt
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

import java.util.{ ArrayList, Comparator }

import scala.annotation.tailrec

// TODO: integrate with IBR

// Differences will be (as compared to IBR in the paper):
// - Descriptors might be `retire`d while still reachable from a ref.
//   - However, they will be EMCAS-finalized in this case.
// - For another difference, see the comment above ThreadContext#write/cas.

/**
 * Efficient Multi-word Compare and Swap:
 * https://arxiv.org/pdf/2008.02527.pdf
 */
private[kcas] object EMCAS extends KCAS { self =>

  // Listing 1 in the paper:

  final object MCASDescriptor {
    // TODO: should always be inlined
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
  ) extends EMCASDescriptorBase with self.Desc with self.Snap {

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

  // Listing 2 in the paper:

  /**
   * A specialized version of `readInternal` from the paper
   *
   * Only returns the actual value (after possibly helping).
   * Cannot be called from an ongoing MCAS operation (but
   * can be called when we're only reading).
   *
   * (The other version of `readInternal`, specialized for
   * an ongoing MCAS operation is inlined into `tryWord` below,
   * see the do-while loop.)
   */
  @tailrec
  private def readValue[A](ref: Ref[A]): A = {
    val o = ref.unsafeTryRead()
    if (o.isInstanceOf[WordDescriptor[_]]) {
      val wd: WordDescriptor[A] = o.asInstanceOf[WordDescriptor[A]]
      val parentStatus = wd.parent.getStatus()
      if (parentStatus eq EMCASStatus.ACTIVE) {
        MCAS(wd.parent, helping = true) // help the other op
        readValue(ref) // retry
      } else if (parentStatus eq EMCASStatus.SUCCESSFUL) {
        wd.nv
      } else { // FAILED
        wd.ov
      }
    } else {
      o
    }
  }

  // Listing 3 in the paper:

  private[choam] override def tryReadOne[A](ref: Ref[A]): A =
    readValue(ref)

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
      var content: A = nullOf[A]
      var value: A = nullOf[A]
      var go = true
      // Read `content`, and `value` if necessary;
      // this is a specialized and inlined version
      // of `readInternal` from the paper. We're
      // using a do-while loop instead of a tail-recursive
      // function (like in the paper), because we may
      // need both `content` and `value`, and returning
      // them would require allocating a tuple (like in
      // the paper).
      do {
        content = wordDesc.address.unsafeTryRead()
        if (equ[Any](content, wordDesc)) {
          // already points to the right place, early return:
          return true // scalastyle:ignore return
        } else {
          // At this point, we're sure that if `content` is a
          // descriptor, then it belongs to another op (not `desc`);
          // because otherwise it would've been equal to `wordDesc`
          // (we're assuming that any WordDescriptor only appears at
          // most once in an MCASDescriptor).
          if (content.isInstanceOf[WordDescriptor[_]]) {
            val wd: WordDescriptor[A] = content.asInstanceOf[WordDescriptor[A]]
            val parentStatus = wd.parent.getStatus()
            if (parentStatus eq EMCASStatus.ACTIVE) {
              MCAS(wd.parent, helping = true) // help the other op
              // Note: we're not "helping" ourselves for sure, see the comment above.
              // Here, we still don't have the value, so the loop must retry.
            } else if (parentStatus eq EMCASStatus.SUCCESSFUL) {
              value = wd.nv
              go = false
            } else {
              value = wd.ov
              go = false
            }
          } else {
            value = content
            go = false
          }
        }
      } while (go)

      if (!equ(value, wordDesc.ov)) {
        // expected value is different
        false
      } else if (desc.getStatus() ne EMCASStatus.ACTIVE) {
        // we have been finalized (by a helping thread), no reason to continue
        true // TODO: we should break from `go`
        // TODO: `true` is not necessarily correct, the helping thread could've finalized us to failed too
      } else {
        if (!wordDesc.address.unsafeTryPerformCas(content, wordDesc.asInstanceOf[A])) {
          tryWord(wordDesc) // retry this word
        } else {
          true
        }
      }
    }

    @tailrec
    def go(words: java.util.Iterator[WordDescriptor[_]]): Boolean = {
      if (words.hasNext) {
        if (tryWord(words.next())) go(words)
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
    if (desc.casStatus(
      EMCASStatus.ACTIVE,
      if (success) EMCASStatus.SUCCESSFUL else EMCASStatus.FAILED
    )) {
      // TODO: We finalized the descriptor, mark it for reclamation:
      // TODO: retireForCleanup(desc)
      success
    } else {
      // someone else finalized the descriptor, must read its status:
      desc.getStatus() eq EMCASStatus.SUCCESSFUL
    }
  }

  private[choam] override def start(): Desc = {
    new MCASDescriptor(new ArrayList(MCASDescriptor.minArraySize))
  }
}
