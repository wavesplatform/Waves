package com.wavesplatform.transaction.validation

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}
import com.wavesplatform.transaction.{TxValidationError, TxVersion, VersionedTransaction}

import scala.util.Try

object TxConstraints {
  // Generic
  def seq[T](value: T)(validations: ValidatedV[Any]*): ValidatedV[T] = {
    validations.map(_.map(_ => value)).fold(Validated.validNel(value)) {
      case (Invalid(leftErrs), Invalid(rightErrs)) => Invalid(leftErrs.concatNel(rightErrs))
      case (invalid @ Invalid(_), _)               => invalid
      case (_, invalid @ Invalid(_))               => invalid
      case (Valid(_), Valid(_))                    => Valid(value)
    }
  }

  def cond(cond: => Boolean, err: => ValidationError): ValidatedNV =
    if (cond) Valid(()) else Invalid(err).toValidatedNel

  def byVersionSet[T <: VersionedTransaction](tx: T)(f: (Set[TxVersion], () => ValidatedV[Any])*): ValidatedV[T] = {
    seq(tx)(f.collect {
      case (v, func) if v.contains(tx.version) =>
        func()
    }: _*)
  }

  def byVersion[T <: VersionedTransaction](tx: T)(f: (TxVersion, () => ValidatedV[Any])*): ValidatedV[T] =
    byVersionSet(tx)(f.map { case (v, f) => (Set(v), f) }: _*)

  def fee(fee: Long): ValidatedV[Long] = {
    Validated
      .condNel(
        fee > 0,
        fee,
        TxValidationError.InsufficientFee()
      )
  }

  def positiveAmount(amount: Long, of: => String): ValidatedV[Long] = {
    Validated
      .condNel(
        amount > 0,
        amount,
        TxValidationError.NonPositiveAmount(amount, of)
      )
  }

  def positiveOrZeroAmount(amount: Long, of: => String): ValidatedV[Long] = {
    Validated
      .condNel(
        amount >= 0,
        amount,
        TxValidationError.NegativeAmount(amount, of)
      )
  }

  def noOverflow(amounts: Long*): ValidatedV[Long] = {
    Try(amounts.fold(0L)(Math.addExact))
      .fold[ValidatedV[Long]](
        _ => TxValidationError.OverflowError.invalidNel,
        _.validNel
      )
  }

  // Transaction specific
  def transferAttachment(allowTyped: Boolean, attachment: Attachment): ValidatedV[Attachment] = {
    this.seq(attachment)(
      cond(attachment.size <= TransferTransaction.MaxAttachmentSize, TxValidationError.TooBigArray),
      if (allowTyped) Valid(attachment)
      else cond(attachment.isInstanceOf[Attachment.Bin] || attachment == Attachment.Empty, TxValidationError.TooBigArray)
    )
  }
}
