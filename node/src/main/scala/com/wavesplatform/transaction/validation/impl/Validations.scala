package com.wavesplatform.transaction.validation.impl

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}
import com.wavesplatform.lang.ValidationError

object Validations {
  def seq[T](value: T)(validations: ValidatedNel[ValidationError, Any]*): ValidatedNel[ValidationError, T] = {
    validations.map(_.map(_ => value)).fold(Validated.validNel(value)) {
      case (Invalid(leftErrs), Invalid(rightErrs)) => Invalid(leftErrs.concatNel(rightErrs))
      case (_, invalid @ Invalid(_))               => invalid
      case (Valid(_), Valid(_))                    => Valid(value)
    }
  }
}
