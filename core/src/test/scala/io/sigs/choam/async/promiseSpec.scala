/*
 * Copyright 2017-2018 Daniel Urban and contributors listed in AUTHORS
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

package io.sigs.choam
package async

import java.util.concurrent.atomic.AtomicLong

import cats.implicits._
import cats.effect.IO

class PromiseSpecNaiveKCAS
  extends PromiseSpec
  with SpecNaiveKCAS

class PromiseSpecCASN
  extends PromiseSpec
  with SpecCASN

class PromiseSpecMCAS
  extends PromiseSpec
  with SpecMCAS

abstract class PromiseSpec extends BaseSpec {

  "Completing an empty promise" should "call all registered callbacks" in {
    val p = Promise[Int].unsafeRun
    val act = p.get.run[IO]
    val tsk = for {
      fut1 <- fs2.async.start(act)
      fut2 <- fs2.async.start(act)
      _ <- IO { Thread.sleep(100L) }
      b <- AsyncReact.lift(React.ret(42) >>> p.tryComplete).run[IO]
      res1 <- fut1
      res2 <- fut2
    } yield (b, res1, res2)
    tsk.unsafeRunSync() should === ((true, 42, 42))
  }

  "Completing a fulfilled promise" should "not be possible" in {
    val p = Promise[Int].unsafeRun
    p.tryComplete.unsafePerform(42) should === (true)
    p.tryComplete.unsafePerform(42) should === (false)
    p.tryComplete.unsafePerform(99) should === (false)
  }

  it should "not call any callbacks" in {
    val p = Promise[Int].unsafeRun
    p.tryComplete.unsafePerform(42) should === (true)
    val cnt = new AtomicLong(0L)
    val act = p.get.map { x =>
      cnt.incrementAndGet()
      x
    }.run[IO]
    val tsk = for {
      res1 <- act
      res2 <- act
      b <- AsyncReact.lift(React.ret(42) >>> p.tryComplete).run[IO]
    } yield (b, res1, res2)
    tsk.unsafeRunSync() should === ((false, 42, 42))
    cnt.get() should === (2L)
  }
}
