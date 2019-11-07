package com.wavesplatform

import com.wavesplatform.lang.ValidationError

import scala.util.Try

package object block {
  private[block] implicit class ValidationOps[A](v: Either[ValidationError, A]) {
    def asTry: Try[A] = v.left.map(ve => new IllegalArgumentException(ve.toString)).toTry
  }
}
