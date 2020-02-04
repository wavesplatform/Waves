package com.wavesplatform.transaction.transfer

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.account.{AddressOrAlias, KeyPair, PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.common.utils.{Base58, EitherExt2}
import com.wavesplatform.crypto
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.description._
import com.wavesplatform.transaction.transfer.MassTransferTransaction.{ParsedTransfer, toJson}
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject, JsValue, Json}

import scala.util.{Either, Try}

case class MassTransferTransaction private (
    assetId: Asset,
    sender: PublicKey,
    transfers: List[ParsedTransfer],
    timestamp: Long,
    fee: Long,
    attachment: Array[Byte],
    proofs: Proofs
) extends ProvenTransaction
    with VersionedTransaction
    with FastHashId {

  override val builder: MassTransferTransaction.type = MassTransferTransaction
  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {

    val assetIdBytes = assetId.byteRepr

    val transferBytes = transfers
      .map { case ParsedTransfer(recipient, amount) => recipient.bytes.arr ++ Longs.toByteArray(amount) }

    Bytes.concat(
      Array(builder.typeId, version),
      sender,
      assetIdBytes,
      Shorts.toByteArray(transfers.size.toShort),
      Bytes.concat(transferBytes: _*),
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee),
      Deser.serializeArray(attachment)
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), proofs.bytes()))

  override val assetFee: (Asset, Long) = (Waves, fee)

  override def jsonBase(): JsObject = {
    super.jsonBase() ++ Json.obj(
      "version"       -> version,
      "assetId"       -> assetId.maybeBase58Repr,
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

  override def checkedAssets(): Seq[IssuedAsset] = assetId match {
    case Waves          => Seq()
    case a: IssuedAsset => Seq(a)
  }

  override def version: Byte = MassTransferTransaction.version
}

object MassTransferTransaction extends TransactionParserFor[MassTransferTransaction] with TransactionParser.OneVersion {

  override val typeId: Byte  = 11
  override val version: Byte = 1
  val MaxTransferCount       = 100

  case class Transfer(
      recipient: String,
      amount: Long
  )

  case class ParsedTransfer(address: AddressOrAlias, amount: Long)

  implicit val transferFormat: Format[Transfer] = Json.format

  override protected def parseTail(bytes: Array[Byte]): Try[TransactionT] = {
    byteTailDescription.deserializeFromByteArray(bytes).flatMap { tx =>
      Try { tx.transfers.map(_.amount).fold(tx.fee)(Math.addExact) }
        .fold(
          ex => Left(OverflowError),
          totalAmount =>
            if (tx.transfers.lengthCompare(MaxTransferCount) > 0) {
              Left(GenericError(s"Number of transfers ${tx.transfers.length} is greater than $MaxTransferCount"))
            } else if (tx.transfers.exists(_.amount < 0)) {
              Left(GenericError("One of the transfers has negative amount"))
            } else if (tx.attachment.length > TransferTransaction.MaxAttachmentSize) {
              Left(TooBigArray)
            } else if (tx.fee <= 0) {
              Left(InsufficientFee())
            } else {
              Right(tx)
            }
        )
        .foldToTry
    }
  }

  def create(
      assetId: Asset,
      sender: PublicKey,
      transfers: List[ParsedTransfer],
      timestamp: Long,
      feeAmount: Long,
      attachment: Array[Byte],
      proofs: Proofs
  ): Either[ValidationError, TransactionT] = {
    Try {
      transfers.map(_.amount).fold(feeAmount)(Math.addExact)
    }.fold(
      ex => Left(OverflowError),
      totalAmount =>
        if (transfers.lengthCompare(MaxTransferCount) > 0) {
          Left(GenericError(s"Number of transfers ${transfers.length} is greater than $MaxTransferCount"))
        } else if (transfers.exists(_.amount < 0)) {
          Left(GenericError("One of the transfers has negative amount"))
        } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
          Left(TooBigArray)
        } else if (feeAmount <= 0) {
          Left(InsufficientFee())
        } else {
          Right(MassTransferTransaction(assetId, sender, transfers, timestamp, feeAmount, attachment, proofs))
        }
    )
  }

  def signed(
      assetId: Asset,
      sender: PublicKey,
      transfers: List[ParsedTransfer],
      timestamp: Long,
      feeAmount: Long,
      attachment: Array[Byte],
      signer: PrivateKey
  ): Either[ValidationError, TransactionT] = {
    create(assetId, sender, transfers, timestamp, feeAmount, attachment, Proofs.empty).right.map { unsigned =>
      unsigned.copy(proofs = Proofs.create(Seq(ByteStr(crypto.sign(signer, unsigned.bodyBytes())))).explicitGet())
    }
  }

  def selfSigned(
      assetId: Asset,
      sender: KeyPair,
      transfers: List[ParsedTransfer],
      timestamp: Long,
      feeAmount: Long,
      attachment: Array[Byte]
  ): Either[ValidationError, TransactionT] = {
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
    (
      PublicKeyBytes(tailIndex(1), "Sender's public key"),
      OptionBytes(index = tailIndex(2), name = "Asset ID", nestedByteEntity = AssetIdBytes(tailIndex(2), "Asset ID")),
      TransfersBytes(tailIndex(3)),
      LongBytes(tailIndex(4), "Timestamp"),
      LongBytes(tailIndex(5), "Fee"),
      BytesArrayUndefinedLength(tailIndex(6), "Attachments", TransferTransaction.MaxAttachmentSize),
      ProofsBytes(tailIndex(7))
    ) mapN {
      case (sender, assetId, transfer, timestamp, fee, attachment, proofs) =>
        MassTransferTransaction(
          assetId = assetId.getOrElse(Waves),
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
