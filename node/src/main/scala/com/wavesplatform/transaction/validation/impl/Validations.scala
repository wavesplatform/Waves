package com.wavesplatform.transaction.validation.impl

import cats.data.Validated.{Invalid, Valid}
import cats.data.{Validated, ValidatedNel}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.validation

import scala.util.Try

object Validations {
  def seq[T](value: T)(validations: ValidatedNel[ValidationError, Any]*): ValidatedNel[ValidationError, T] = {
    validations.map(_.map(_ => value)).fold(Validated.validNel(value)) {
      case (Invalid(leftErrs), Invalid(rightErrs)) => Invalid(leftErrs.concatNel(rightErrs))
      case (invalid @ Invalid(_), _)               => invalid
      case (_, invalid @ Invalid(_))               => invalid
      case (Valid(_), Valid(_))                    => Valid(value)
    }
  }

  def cond(cond: => Boolean, err: => ValidationError): ValidatedNel[ValidationError, Unit] =
    if (cond) Valid(()) else Invalid(err).toValidatedNel

  def tryDo(cond: => Unit, err: => ValidationError): ValidatedNel[ValidationError, Unit] =
    if (Try(cond).isSuccess) Valid(()) else Invalid(err).toValidatedNel

  def fee(fee: Long): ValidatedNel[ValidationError, Long] =
    validation.validateFee(fee)
}
