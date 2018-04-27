package scorex.transaction

import cats.data.{NonEmptyList, Validated, ValidatedNel}
import cats.implicits._
import com.wavesplatform.state.DataEntry
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.transfer.MassTransferTransaction.ParsedTransfer
import scorex.transaction.transfer._

import scala.util.Try

package object validation {
  val MaxDescriptionLength = 1000
  val MaxAssetNameLength   = 16
  val MinAssetNameLength   = 4
  val MaxDecimals          = 8
  val MaxTransferCount     = 100
  val MaxEntryCount        = 100

  def mkGeneric(errNel: NonEmptyList[ValidationError]): ValidationError = GenericError(errNel.toString)

  type Validated[A] = ValidatedNel[ValidationError, A]

  def validateVersion(supported: Set[Byte], version: Byte): Validated[Byte] = {
    Validated
      .condNel(
        supported contains version,
        version,
        ValidationError.UnsupportedVersion(version)
      )
  }

  def validateFee(fee: Long): Validated[Long] = {
    Validated
      .condNel(
        fee > 0,
        fee,
        ValidationError.InsufficientFee()
      )
  }

  def validateMinFee(fee: Long, of: String): Validated[Long] = {
    Validated
      .condNel(
        fee > 0,
        fee,
        ValidationError.NegativeMinFee(fee, of)
      )
  }

  def validateAmount(amount: Long, of: String): Validated[Long] = {
    Validated
      .condNel(
        amount > 0,
        amount,
        ValidationError.NegativeAmount(amount, of)
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

  def validateTransfers(transfers: List[ParsedTransfer]): Validated[List[ParsedTransfer]] = {
    val checkedAmounts: Validated[Unit] =
      Validated
        .condNel(
          transfers.exists(_.amount < 0),
          (),
          GenericError("One of transfers contains negative amount")
        )

    val checkedLength: Validated[Unit] =
      Validated.condNel(
        transfers.lengthCompare(MaxTransferCount) > 0,
        (),
        ValidationError.GenericError(s"Number of transfers is greater than $MaxTransferCount")
      )

    (checkedLength, checkedAmounts)
      .mapN { case (_, _) => transfers }
  }

  def validateDecimals(decimals: Byte): Validated[Byte] = {
    Validated
      .condNel(
        decimals >= 0 && decimals <= MaxDecimals,
        decimals,
        ValidationError.TooBigArray
      )
  }

  def validateDataEntries(entries: List[DataEntry[_]]): Validated[List[DataEntry[_]]] = {
    Validated
      .condNel(
        entries.length < MaxEntryCount && entries.forall(_.valid),
        entries,
        ValidationError.TooBigArray
      )
  }
}
