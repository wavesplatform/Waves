package scorex.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs, Shorts}
import com.wavesplatform.state2.ByteStr
import monix.eval.Coeval
import play.api.libs.json.{Format, JsObject, JsValue, Json}
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.EllipticCurveImpl
import scorex.crypto.encode.Base58
import scorex.serialization.Deser
import scorex.transaction.TransactionParser._
import scorex.transaction.ValidationError.Validation
import scorex.transaction._
import scorex.transaction.assets.MassTransferTransaction.{ParsedTransfer, toJson}

import scala.util.{Either, Failure, Success, Try}

case class MassTransferTransaction private(assetId: Option[AssetId],
                                           sender: PublicKeyAccount,
                                           transfers: List[ParsedTransfer],
                                           timestamp: Long,
                                           fee: Long,
                                           attachment: Array[Byte],
                                           signature: ByteStr) extends SignedTransaction with FastHashId
{
  override val transactionType: TransactionType.Value = TransactionType.MassTransferTransaction

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val assetIdBytes = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val transferBytes = transfers
      .map { case ParsedTransfer(recipient, amount) => recipient.bytes.arr ++ Longs.toByteArray(amount) }
      .fold(Array())(_ ++ _)

    Bytes.concat(
      Array(transactionType.id.toByte),
      sender.publicKey,
      assetIdBytes,
      Shorts.toByteArray(transfers.size.toShort),
      transferBytes,
      Longs.toByteArray(timestamp),
      Longs.toByteArray(fee),
      Deser.serializeArray(attachment))
  }

  override def jsonBase(): JsObject = super.jsonBase() ++ Json.obj(
    "assetId" -> assetId.map(_.base58),
    "attachment" -> Base58.encode(attachment),
    "transferCount" -> transfers.size,
    "totalAmount" -> transfers.map(_.amount).sum)


  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj("transfers" -> toJson(transfers))
  }

  def compactJson(recipient: AddressOrAlias): JsObject = jsonBase() ++ Json.obj(
      "transfers" -> toJson(transfers.filter(_.address == recipient)))

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(bodyBytes(), signature.arr))
}

object MassTransferTransaction {
  val MaxTransferCount = 100

  case class Transfer(recipient: String, amount: Long)
  case class ParsedTransfer(address: AddressOrAlias, amount: Long)
  implicit val transferFormat: Format[Transfer] = Json.format

  def parseTail(bytes: Array[Byte]): Try[MassTransferTransaction] = Try {
    val sender = PublicKeyAccount(bytes.slice(0, KeyLength))
    val (assetIdOpt, s0) = Deser.parseByteArrayOption(bytes, KeyLength, AssetIdLength)
    val transferCount = Shorts.fromByteArray(bytes.slice(s0, s0 + 2))

    def readTransfer(offset: Int): (Validation[ParsedTransfer], Int) = {
      AddressOrAlias.fromBytes(bytes, offset) match {
        case Right((addr, ofs)) =>
          val amount = Longs.fromByteArray(bytes.slice(ofs, ofs + 8))
          (Right[ValidationError, ParsedTransfer](ParsedTransfer(addr, amount)), ofs + 8)
        case Left(e) => (Left(e), offset)
      }
    }
    val transfersList: List[(Validation[ParsedTransfer], Int)] =
      List.iterate(readTransfer(s0 + 2), transferCount) { case (_, offset) => readTransfer(offset) }

    val s1 = transfersList.lastOption.map(_._2).getOrElse(s0 + 2)
    val tx: Validation[MassTransferTransaction] = for {
      transfers <- transfersList.map { case (ei, _) => ei }.sequence
      timestamp = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
      feeAmount = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
      (attachment, s2) = Deser.parseArraySize(bytes, s1 + 16)
      signature = ByteStr(bytes.slice(s2, s2 + SignatureLength))
      mtt <- MassTransferTransaction.create(assetIdOpt.map(ByteStr(_)), sender, transfers, timestamp, feeAmount, attachment, signature)
    } yield mtt
    tx.fold(left => Failure(new Exception(left.toString)), right => Success(right))
  }.flatten

  def create(assetId: Option[AssetId],
             sender: PublicKeyAccount,
             transfers: List[ParsedTransfer],
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte],
             signature: ByteStr): Either[ValidationError, MassTransferTransaction] = {
    Try { transfers.map(_.amount).fold(feeAmount)(Math.addExact) }.fold(
      ex => Left(ValidationError.OverflowError),
      totalAmount =>
        if (transfers.lengthCompare(MaxTransferCount) > 0) {
          Left(ValidationError.GenericError(s"Number of transfers is greater than $MaxTransferCount"))
        } else if (transfers.exists(_.amount < 0)) {
          Left(ValidationError.GenericError("One of the transfers has negative amount"))
        } else if (attachment.length > TransferTransaction.MaxAttachmentSize) {
          Left(ValidationError.TooBigArray)
        } else if (feeAmount <= 0) {
          Left(ValidationError.InsufficientFee)
        } else {
          Right(MassTransferTransaction(assetId, sender, transfers, timestamp, feeAmount, attachment, signature))
        }
    )
  }

  def create(assetId: Option[AssetId],
             sender: PrivateKeyAccount,
             transfers: List[ParsedTransfer],
             timestamp: Long,
             feeAmount: Long,
             attachment: Array[Byte]): Either[ValidationError, MassTransferTransaction] = {
    create(assetId, sender, transfers, timestamp, feeAmount, attachment, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(EllipticCurveImpl.sign(sender, unsigned.bodyBytes())))
    }
  }

  def parseTransfersList(transfers: List[Transfer]): Validation[List[ParsedTransfer]] = {
    transfers.traverse { case Transfer(recipient, amount) =>
      AddressOrAlias.fromString(recipient).map(ParsedTransfer(_, amount))
    }
  }

  private def toJson(transfers: List[ParsedTransfer]): JsValue =
    Json.toJson(transfers.map { case ParsedTransfer(address, amount) => Transfer(address.stringRepr, amount) })
}
