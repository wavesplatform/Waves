package com.wavesplatform.transaction.validation

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer.TransferTransaction

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
  def transferAttachment(attachment: Array[Byte]): ValidatedV[Array[Byte]] = {
    Validated
      .condNel(
        attachment.length <= TransferTransaction.MaxAttachmentSize,
        attachment,
        TxValidationError.TooBigArray
      )
  }

  def assetName(name: Array[Byte]): ValidatedV[Array[Byte]] = {
    Validated
      .condNel(
        name.length >= IssueTransaction.MinAssetNameLength && name.length <= IssueTransaction.MaxAssetNameLength,
        name,
        TxValidationError.InvalidName
      )
  }

  def assetDescription(description: Array[Byte]): ValidatedV[Array[Byte]] = {
    Validated
      .condNel(
        description.length <= IssueTransaction.MaxAssetDescriptionLength,
        description,
        TxValidationError.TooBigArray
      )
  }

  def assetDecimals(decimals: Byte): ValidatedV[Byte] = {
    Validated
      .condNel(
        decimals >= 0 && decimals <= IssueTransaction.MaxAssetDecimals,
        decimals,
        TxValidationError.TooBigArray
      )
  }
}
