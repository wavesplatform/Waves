package com.wavesplatform.transaction.validation.impl

import cats.data.Validated.{Invalid, Valid}
import cats.data.ValidatedNel
import com.wavesplatform.lang.ValidationError

object Validations {
  def seq[T](value: T)(validations: ValidatedNel[ValidationError, _]*): ValidatedNel[ValidationError, T] = {
    validations.reduce {
      case (Invalid(leftErrs), Invalid(rightErrs)) => Invalid(leftErrs ::: rightErrs)
      case (_, invalid @ Invalid(_))               => invalid
      case (Valid(_), Valid(_))                    => Valid(value)
    }
  }
}
