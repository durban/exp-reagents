/*
 * Copyright 2017 Daniel Urban and contributors listed in AUTHORS
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

import scala.concurrent.ExecutionContext

import org.scalatest.{ FlatSpecLike, Matchers }
import org.scalactic.TypeCheckedTripleEquals

trait KCASImplSpec {
  implicit def kcasImpl: kcas.KCAS
}

trait BaseSpec extends KCASImplSpec with FlatSpecLike with Matchers with TypeCheckedTripleEquals {

  implicit val ec: ExecutionContext =
    ExecutionContext.global
}

trait SpecNaiveKCAS extends KCASImplSpec {
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.NaiveKCAS
}

trait SpecCASN extends KCASImplSpec {
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.CASN
}

trait SpecMCAS extends KCASImplSpec {
  implicit override def kcasImpl: kcas.KCAS =
    kcas.KCAS.MCAS
}
