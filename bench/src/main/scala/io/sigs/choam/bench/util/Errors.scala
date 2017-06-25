package io.sigs.choam
package bench
package util

import scala.util.control.NoStackTrace

object Errors {
  object EmptyQueue extends AssertionError with NoStackTrace
  object EmptyStack extends AssertionError with NoStackTrace
}
