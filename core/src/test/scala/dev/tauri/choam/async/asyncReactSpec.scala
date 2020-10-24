/*
 * Copyright 2017-2020 Daniel Urban and contributors listed in AUTHORS
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
package async

import java.util.concurrent.atomic.{ AtomicLong, AtomicBoolean, AtomicReference }
import java.util.concurrent.CountDownLatch

import cats.effect.IO
import cats.implicits._

import kcas.Ref

class AsyncReactSpecNaiveKCAS
  extends AsyncReactSpec
  with SpecNaiveKCAS

class AsyncReactSpecCASN
  extends AsyncReactSpec
  with SpecCASN

class AsyncReactSpecMCAS
  extends AsyncReactSpec
  with SpecMCAS

abstract class AsyncReactSpec extends BaseSpec {

  import AsyncReact._

  // TODO: migrate these to `PromiseSpec`

  "running pure" should "return the pure value" in {
    pure(42).run[IO].unsafeRunSync() should === (42)
  }

  "cancelling pure" should "be ineffective" in {
    val r = pure(42)
    val tsk = for {
      ac <- r.runCancellable[IO]
      (act, cancel) = ac
      fib <- act.start
      _ <- cancel
      r <- fib.join
    } yield r
    tsk.unsafeRunSync() should === (Some(42))
  }

  "running delay" should "execute the side-effecting code" in {
    val r = new AtomicLong(42L)
    val act = delay { r.getAndIncrement() }
    act.run[IO].unsafeRunSync() should === (42L)
    act.run[IO].unsafeRunSync() should === (43L)
    val tsk = act.run[IO]
    tsk.unsafeRunSync() should === (44L)
    tsk.unsafeRunSync() should === (45L)
  }

  "cancelling delay" should "work at the first bind" in {
    val latch = new CountDownLatch(1)
    val cnt = new AtomicLong(0L)
    val r = pure(42).flatMap { x =>
      cnt.incrementAndGet()
      pure(x)
    }.flatMap { x =>
      cnt.incrementAndGet()
      delay {
        latch.await()
        x
      }
    }.flatMap { x =>
      delay {
        cnt.incrementAndGet()
        x
      }
    }
    val tsk = for {
      ac <- r.runCancellable[IO]
      (act, cancel) = ac
      fib <- act.start
      _ <- cancel
      _ <- IO { latch.countDown() }
      r <- fib.join
    } yield r
    tsk.unsafeRunSync() should === (None)
    cnt.get() should === (2L)
  }

  "running async" should "complete when the callback is called" in {
    val r = new AtomicLong(42L)
    val act = async[Long] { cb =>
      Thread.sleep(100L)
      cb(r.getAndIncrement())
      _ => ()
    }
    act.run[IO].unsafeRunSync() should === (42L)
    act.run[IO].unsafeRunSync() should === (43L)
    val tsk = act.run[IO]
    tsk.unsafeRunSync() should === (44L)
    tsk.unsafeRunSync() should === (45L)
  }

  it should "work for async callbacks" in {
    val ph = new AtomicReference[scala.concurrent.Promise[Int]](null)
    val cancelled = new AtomicBoolean(false)
    val latch = new CountDownLatch(1)
    val act = async[Int] { cb =>
      val promise = scala.concurrent.Promise[Int]()
      promise.future.foreach(cb)
      ph.set(promise)
      latch.countDown()
      _ => { cancelled.set(true) }
    }
    val task = for {
      tc <- act.runCancellable[IO]
      (tsk, cancel) = tc
      fib <- tsk.start
      _ <- IO { latch.await() }
      _ <- IO { ph.get().complete(scala.util.Success(42)) }
      r <- fib.join
      _ <- cancel // ineffective
    } yield r
    task.unsafeRunSync() should === (Some(42))
    cancelled.get() should === (false)
  }

  it should "be protected against the user calling the callback twice" in {
    val r = new AtomicLong(42L)
    val c = new AtomicLong(0L)
    val act = async[Long] { cb =>
      Thread.sleep(100L)
      val res = r.getAndIncrement()
      cb(res)
      cb(res)
      _ => ()
    }.map { res =>
      c.getAndIncrement()
      res
    }
    act.runCancellable[IO].flatMap(_._1).unsafeRunSync() should === (Some(42L))
    c.get() should === (1L)
    act.runCancellable[IO].flatMap(_._1).unsafeRunSync() should === (Some(43L))
    c.get() should === (2L)
    val tsk = act.runCancellable[IO].flatMap(_._1)
    c.get() should === (2L)
    tsk.unsafeRunSync() should === (Some(44L))
    c.get() should === (3L)
    tsk.unsafeRunSync() should === (Some(45L))
    c.get() should === (4L)
  }

  "cancelling async" should "work, if the callback was not called yet" in {
    val latch1 = new CountDownLatch(1)
    val latch2 = new CountDownLatch(1)
    val cancelLatch = new CountDownLatch(1)
    val cnt = new AtomicLong(0L)
    val act = async[Long] { cb =>
      latch2.countDown()
      latch1.await()
      cb(cnt.getAndIncrement())
      _ => {
        cancelLatch.countDown()
      }
    }.map { x =>
      cnt.getAndIncrement()
      x
    }
    val task = for {
      tc <- act.runCancellable[IO]
      (tsk, cancel) = tc
      fib <- tsk.start
      _ <- IO { latch2.await() }
      _ <- cancel
      _ <- IO { latch1.countDown() }
      r <- fib.join
    } yield r
    task.unsafeRunSync() should === (None)
    cnt.get() should === (1L)
    cancelLatch.await()
  }

  it should "work for truly asynchronous stuff" in {
    val ph = new AtomicReference[scala.concurrent.Promise[Int]](null)
    val cancelled = new AtomicBoolean(false)
    val latch = new CountDownLatch(1)
    val act = async[Int] { cb =>
      val promise = scala.concurrent.Promise[Int]()
      promise.future.foreach(cb)
      ph.set(promise)
      latch.countDown()
      _ => { cancelled.set(true) }
    }
    val task = for {
      tc <- act.runCancellable[IO]
      (tsk, cancel) = tc
      fib <- tsk.start
      _ <- IO { latch.await() }
      _ <- cancel
      r <- fib.join
    } yield r
    task.unsafeRunSync() should === (None)
    cancelled.get() should === (true)
  }

  it should "handle multiple blocks correctly" in {
    val ph = new AtomicReference[scala.concurrent.Promise[Int]](null)
    val latch = new CountDownLatch(1)
    val cancelled = new AtomicBoolean(false)
    val cnt = new AtomicLong(0L)
    val act = async[Int] { cb =>
      cnt.incrementAndGet()
      val promise = scala.concurrent.Promise[Int]()
      promise.future.foreach(cb)
      ph.set(promise)
      latch.countDown()
      _ => { cancelled.set(true) }
    }
    val task = for {
      tc <- act.runCancellable[IO]
      (block, _) = tc
      _ <- IO { latch.await() }
      fib <- block.start
      _ <- IO { ph.get().complete(scala.util.Success(42)) }
      r1 <- fib.join
      r2 <- block
      r3 <- block
    } yield (r1, r2, r3)
    task.unsafeRunSync() should === ((Some(42), Some(42), Some(42)))
    cancelled.get() should === (false)
    cnt.get() should === (1L)
  }

  it should "handle multiple cancels correctly" in {
    val ph = new AtomicReference[scala.concurrent.Promise[Int]](null)
    val latch = new CountDownLatch(1)
    val cancelled = new AtomicLong(0L)
    val cnt = new AtomicLong(0L)
    val act = async[Int] { cb =>
      cnt.incrementAndGet()
      val promise = scala.concurrent.Promise[Int]()
      promise.future.foreach(cb)
      ph.set(promise)
      latch.countDown()
      _ => {
        cancelled.incrementAndGet()
        ()
      }
    }
    val task = for {
      tc <- act.runCancellable[IO]
      (block, cancel) = tc
      _ <- IO { latch.await() }
      fib1 <- block.start
      fib2 <- block.start
      c1 <- cancel.start
      c2 <- cancel.start
      _ <- c1.join
      _ <- c2.join
      r1 <- fib1.join
      r2 <- fib2.join
      r3 <- block
      r4 <- block
    } yield (r1, r2, r3, r4)
    task.unsafeRunSync() should === ((None, None, None, None))
    cancelled.get() should === (1L)
    cnt.get() should === (1L)
  }

  "running lift" should "perform the reaction" in {
    val ref = Ref.mk(42L)
    val r = ref.modify(_ + 1L)
    val act = lift(r)
    act.run[IO].unsafeRunSync() should === (42L)
    act.run[IO].unsafeRunSync() should === (43L)
    val tsk = act.run[IO]
    tsk.unsafeRunSync() should === (44L)
    tsk.unsafeRunSync() should === (45L)
  }

  "running kcas" should "return the k-CAS impl" in {
    AsyncReact.kcas.run[IO].unsafeRunSync() shouldBe theSameInstanceAs (this.kcasImpl)
  }

  "CancelRef" should "perform addCanceller correctly" in {
    val cr = CancelRef.mk()
    val called = new AtomicLong(0L)
    val i1 = new Id
    val c1: Cancel = { _ =>
      called.incrementAndGet()
      ()
    }
    cr.addCanceller.unsafePerform(new Id -> c1) should === (CannotModify)
    called.get() should === (0L)
    cr.addStub.unsafePerform(i1)
    cr.addCanceller.unsafePerform(i1 -> c1) should === (Ok)
    called.get() should === (0L)
    cr.cancel.unsafeRun
    called.get() should === (1L)
    cr.addCanceller.unsafePerform(new Id -> c1) should === (AlreadyCancelled)
    called.get() should === (1L)
  }

  it should "perform removeCanceller correctly" in {
    val cr = CancelRef.mk()
    val called = new AtomicLong(0L)
    val i1 = new Id
    val c1: Cancel = { _ =>
      called.incrementAndGet()
      ()
    }
    cr.removeCanceller.unsafePerform(i1) should === (CannotModify)
    cr.addStub.unsafePerform(i1)
    cr.addCanceller.unsafePerform(i1 -> c1) should === (Ok)
    called.get() should === (0L)
    cr.removeCanceller.unsafePerform(i1) should === (Ok)
    called.get() should === (0L)
    cr.addStub.unsafePerform(i1)
    cr.addCanceller.unsafePerform(i1 -> c1) should === (Ok)
    called.get() should === (0L)
    cr.cancel.unsafeRun
    called.get() should === (1L)
    cr.removeCanceller.unsafePerform(i1) should === (AlreadyCancelled)
    called.get() should === (1L)
  }
}
