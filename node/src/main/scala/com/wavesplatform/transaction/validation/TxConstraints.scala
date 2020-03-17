package com.wavesplatform.transaction.validation

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import com.google.protobuf.ByteString
import com.wavesplatform.account.AddressOrAlias
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TxValidationError.GenericError
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.transfer.{Attachment, TransferTransaction}
import com.wavesplatform.transaction.{Asset, TxValidationError, TxVersion, VersionedTransaction}

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

  def chainIds(chainId: Byte, ids: Byte*): ValidatedV[Byte] = {
    val chainIds = ids.distinct
    if (chainIds.length <= 1 && chainIds.headOption.forall(_ == chainId)) Valid(chainId)
    else GenericError(s"One of chain ids not match: $ids").invalidNel
  }

  def addressChainId(addr: AddressOrAlias, chainId: Byte): ValidatedV[AddressOrAlias] =
    Validated
      .condNel(
        addr.chainId == chainId,
        addr,
        GenericError("Address or alias from other network")
      )

  // Transaction specific
  def transferAttachment(allowTyped: Boolean, attachment: Option[Attachment]): ValidatedV[Option[Attachment]] = {
    import Attachment.AttachmentExt
    this.seq(attachment)(
      cond(attachment.toBytes.length <= TransferTransaction.MaxAttachmentSize, TxValidationError.TooBigArray),
      cond(attachment match {
        case Some(Attachment.Bin(_)) | None => true
        case _                              => allowTyped
      }, GenericError("Typed attachment not allowed"))
    )
  }

  def asset[A <: Asset](asset: A): ValidatedV[A] = {
    asset.fold(Validated.validNel[ValidationError, A](asset)) { ia =>
      Validated
        .condNel(
          ia.id.length == com.wavesplatform.crypto.DigestLength,
          asset,
          TxValidationError.InvalidAssetId
        )
    }
  }

  def assetName(name: ByteString): ValidatedV[ByteString] =
    Validated
      .condNel(
        name.size >= IssueTransaction.MinAssetNameLength && name.size <= IssueTransaction.MaxAssetNameLength,
        name,
        TxValidationError.InvalidName
      )

  def assetDescription(description: ByteString): ValidatedV[ByteString] =
    Validated
      .condNel(
        description.size <= IssueTransaction.MaxAssetDescriptionLength,
        description,
        TxValidationError.TooBigArray
      )
}
