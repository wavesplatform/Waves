package com.wavesplatform.transaction.transfer

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.ValidationError.Validation
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{ParsedTransfer, toJson}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject, JsValue, Json}

import scala.annotation.meta.field
import scala.util.{Either, Try}

case class MassTransferTransaction private (assetId: Option[AssetId],
                                            sender: PublicKeyAccount,
                                            transfers: List[ParsedTransfer],
                                            timestamp: Long,
                                            fee: Long,
                                            attachment: Array[Byte],
                                            proofs: Proofs)
    extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: MassTransferTransaction.type = MassTransferTransaction
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {

    val assetIdBytes = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))

    val transferBytes = transfers
      .map { case ParsedTransfer(recipient, amount) => recipient.bytes.arr ++ Longs.toByteArray(amount) }
      .fold(Array())(_ ++ _)

    Bytes.concat(
      Array(builder.typeId, version),
      sender.publicKey,
      assetIdBytes,
      Shorts.toByteArray(transfers.size.toShort),
      transferBytes,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee),
      Deser.serializeArray(attachment)
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override def jsonBase(): JsObject = {
    super.jsonBase() ++ Json.obj(
      "version"       -> version,
      "assetId"       -> assetId.map(_.base58),
      "attachment"    -> Base58.encode(attachment),
      "transferCount" -> transfers.size,
      "totalAmount"   -> transfers.map(_.amount).sum
    )
  }

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj("transfers" -> toJson(transfers))
  }

  def compactJson(recipients: Set[AddressOrAlias]): JsObject =
    jsonBase() ++ Json.obj("transfers" -> toJson(transfers.filter(t => recipients.contains(t.address))))

  override def checkedAssets(): Seq[AssetId] = assetId.toSeq
  override def version: Byte                 = MassTransferTransaction.version
}

object MassTransferTransaction extends TransactionParserFor[MassTransferTransaction] with TransactionParser.OneVersion {

  override val typeId: Byte  = 11
  override val version: Byte = 1
  val MaxTransferCount       = 100

  @ApiModel
  case class Transfer(
      @(ApiModelProperty @field)(dataType = "string", example = "3Mciuup51AxRrpSz7XhutnQYTkNT9691HAk", required = true) recipient: String,
      @(ApiModelProperty @field)(dataType = "long", example = "3000000000", required = true) amount: Long)

  case class ParsedTransfer(address: AddressOrAlias, amount: Long)

  implicit val transferFormat: Format[Transfer] = Json.format

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Try { tx.transfers.map(_.amount).fold(tx.fee)(Math.addExact) }
        .fold(
          ex => Left(ValidationError.OverflowError),
          totalAmount =>
            if (tx.transfers.lengthCompare(MaxTransferCount) > 0) {
              Left(ValidationError.GenericError(s"Number of transfers ${tx.transfers.length} is greater than $MaxTransferCount"))
            } else if (tx.transfers.exists(_.amount < 0)) {
              Left(ValidationError.GenericError("One of the transfers has negative amount"))
            } else if (tx.attachment.length > TransferTransaction.MaxAttachmentSize) {
              Left(ValidationError.TooBigArray)
            } else if (tx.fee <= 0) {
              Left(ValidationError.InsufficientFee())
            } else {
              Right(tx)
          }
        )
        .foldToTry
    }
  }

  def create(assetId: Option[AssetId],
             sender: PublicKeyAccount,
             transfers: List[ParsedTransfer],
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             proofs: Proofs): Either[ValidationError, TransactionT] = {
    Try {
      transfers.map(_.amount).fold(feeAmount)(Math.addExact)
    }.fold(
      ex => Left(ValidationError.OverflowError),
      totalAmount =>
        if (transfers.lengthCompare(MaxTransferCount) > 0) {
          Left(ValidationError.GenericError(s"Number of transfers ${transfers.length} is greater than $MaxTransferCount"))
        } else if (transfers.exists(_.amount < 0)) {
          Left(ValidationError.GenericError("One of the transfers has negative amount"))
        } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
          Left(ValidationError.TooBigArray)
        } else if (feeAmount <= 0) {
          Left(ValidationError.InsufficientFee())
        } else {
          Right(MassTransferTransaction(assetId, sender, transfers, timestamp, feeAmount, attachment, proofs))
      }
    )
  }

  def signed(assetId: Option[AssetId],
             sender: PublicKeyAccount,
             transfers: List[ParsedTransfer],
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             signer: PrivateKeyAccount): Either[ValidationError, TransactionT] = {
    create(assetId, sender, transfers, timestamp, feeAmount, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(assetId: Option[AssetId],
                 sender: PrivateKeyAccount,
                 transfers: List[ParsedTransfer],
                 timestamp: Long,
                 feeAmount: Long,
                 attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    signed(assetId, sender, transfers, timestamp, feeAmount, attachment, sender)
  }

  def parseTransfersList(transfers: List[Transfer]): Validation[List[ParsedTransfer]] = {
    transfers.traverse {
      case Transfer(recipient, amount) =>
        AddressOrAlias.fromString(recipient).map(ParsedTransfer(_, amount))
    }
  }

  private def toJson(transfers: List[ParsedTransfer]): JsValue = {
    Json.toJson(transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) })
  }

  val byteTailDescription: ByteEntity[MassTransferTransaction] = {
    (PublicKeyAccountBytes(tailIndex(1), "Sender's public key") ~
      OptionAssetIdBytes(tailIndex(2), "Asset") ~
      TransfersBytes(tailIndex(3)) ~
      LongBytes(tailIndex(4), "Timestamp") ~
      LongBytes(tailIndex(5), "Fee") ~
      BytesArrayUndefinedLength(tailIndex(6), "Attachments") ~
      ProofsBytes(tailIndex(7))).map {
      case ((((((sender, assetId), transfer), timestamp), fee), attachment), proofs) =>
        MassTransferTransaction(
          assetId = assetId,
          sender = sender,
          transfers = transfer,
          timestamp = timestamp,
          fee = fee,
          attachment = attachment,
          proofs = proofs
        )
    }
  }
}
