package com.wavesplatform.transaction

import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError

package object validation {
  type ValidatedV[A] = ValidatedNel[ValidationError, A]
  type ValidatedNV   = ValidatedV[Unit]
}
