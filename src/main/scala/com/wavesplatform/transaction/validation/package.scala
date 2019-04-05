package com.wavesplatform.transaction

import cats.data.{Validated, ValidatedNel}
import cats.implicits._
import cats.{Order => _}
import com.wavesplatform.transaction.transfer._

import scala.util.Try

package object validation {
  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8
  val MaxTransferCount     = 100
  val MaxEntryCount        = 100

  type Validated[A] = ValidatedNel[ValidationError, A]

  def validateFee(fee: Long): Validated[Long] = {
    Validated
      .condNel(
        fee > 0,
        fee,
        ValidationError.InsufficientFee()
      )
  }

  def validateAmount(amount: Long, of: => String): Validated[Long] = {
    Validated
      .condNel(
        amount > 0,
        amount,
        ValidationError.NonPositiveAmount(amount, of)
      )
  }

  def validateSum(amounts: Seq[Long]): Validated[Long] = {
    Try(amounts.tail.fold(amounts.head)(Math.addExact))
      .fold[Validated[Long]](
        _ => ValidationError.OverflowError.invalidNel,
        _.validNel
      )
  }

  def validateName(name: Array[Byte]): Validated[Array[Byte]] = {
    Validated
      .condNel(
        name.length >= MinAssetNameLength && name.length <= MaxAssetNameLength,
        name,
        ValidationError.InvalidName
      )
  }

  def validateDescription(description: Array[Byte]): Validated[Array[Byte]] = {
    Validated
      .condNel(
        description.length <= MaxDescriptionLength,
        description,
        ValidationError.TooBigArray
      )
  }

  def validateAttachment(attachment: Array[Byte]): Validated[Array[Byte]] = {
    Validated
      .condNel(
        attachment.length <= TransferTransaction.MaxAttachmentSize,
        attachment,
        ValidationError.TooBigArray
      )
  }

  def validateDecimals(decimals: Byte): Validated[Byte] = {
    Validated
      .condNel(
        decimals >= 0 && decimals <= MaxDecimals,
        decimals,
        ValidationError.TooBigArray
      )
  }
}
