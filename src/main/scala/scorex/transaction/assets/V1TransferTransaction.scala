package scorex.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.crypto
import com.wavesplatform.state.ByteStr
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import scorex.account.{AddressOrAlias, PrivateKeyAccount, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.crypto.signatures.Curve25519.{KeyLength, SignatureLength}
import scorex.serialization.Deser
import scorex.transaction._

import scala.util.{Failure, Success, Try}
import validation._
import cats.implicits._

trait TransferTransaction extends ProvenTransaction {
  def assetId: Option[AssetId]
  def recipient: AddressOrAlias
  def amount: Long
  def feeAssetId: Option[AssetId]
  def fee: Long
  def attachment: Array[Byte]
  def version: Byte

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "recipient"  -> recipient.stringRepr,
      "assetId"    -> assetId.map(_.base58),
      "feeAssetId" -> feeAssetId.map(_.base58),
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    ))

}

object TransferTransaction {

  def validate(amount: Long, feeAmount: Long, attachment: Array[Byte]): Either[ValidationError, Unit] = {
    (validateAmount(amount, "assets"), validateFee(feeAmount), validateAttachment(attachment), validateSum(Seq(amount, feeAmount)))
      .mapN { case _ => () }
      .toEither
      .leftMap(_.head)
  }

}

case class V1TransferTransaction private (assetId: Option[AssetId],
                                          sender: PublicKeyAccount,
                                          recipient: AddressOrAlias,
                                          amount: Long,
                                          timestamp: Long,
                                          feeAssetId: Option[AssetId],
                                          fee: Long,
                                          attachment: Array[Byte],
                                          signature: ByteStr)
    extends TransferTransaction
    with SignedTransaction
    with FastHashId {

  override val builder: TransactionParser = V1TransferTransaction

  override val assetFee: (Option[AssetId], Long) = (feeAssetId, fee)

  val bodyBytes: Coeval[Array[Byte]] = Coeval.evalOnce {
    val timestampBytes  = Longs.toByteArray(timestamp)
    val assetIdBytes    = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val amountBytes     = Longs.toByteArray(amount)
    val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val feeBytes        = Longs.toByteArray(fee)

    Bytes.concat(
      Array(builder.typeId),
      sender.publicKey,
      assetIdBytes,
      feeAssetIdBytes,
      timestampBytes,
      amountBytes,
      feeBytes,
      recipient.bytes.arr,
      Deser.serializeArray(attachment)
    )
  }

  override val bytes: Coeval[Array[Byte]] = Coeval.evalOnce(Bytes.concat(Array(builder.typeId), signature.arr, bodyBytes()))

  override val version: Byte = 1: Byte
}

object V1TransferTransaction extends TransactionParserFor[V1TransferTransaction] with TransactionParser.HardcodedVersion1 {

  override val typeId: Byte = 4

  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  override protected def parseTail(version: Byte, bytes: Array[Byte]): Try[TransactionT] =
    Try {
      val signature = ByteStr(bytes.slice(0, SignatureLength))
      val txId      = bytes(SignatureLength)
      require(txId == typeId, s"Signed tx id is not match")
      val sender              = PublicKeyAccount(bytes.slice(SignatureLength + 1, SignatureLength + KeyLength + 1))
      val (assetIdOpt, s0)    = Deser.parseByteArrayOption(bytes, SignatureLength + KeyLength + 1, AssetIdLength)
      val (feeAssetIdOpt, s1) = Deser.parseByteArrayOption(bytes, s0, AssetIdLength)
      val timestamp           = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
      val amount              = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
      val feeAmount           = Longs.fromByteArray(bytes.slice(s1 + 16, s1 + 24))

      (for {
        recRes <- AddressOrAlias.fromBytes(bytes, s1 + 24)
        (recipient, recipientEnd) = recRes
        (attachment, _)           = Deser.parseArraySize(bytes, recipientEnd)
        tt <- V1TransferTransaction.create(assetIdOpt.map(ByteStr(_)),
                                           sender,
                                           recipient,
                                           amount,
                                           timestamp,
                                           feeAssetIdOpt.map(ByteStr(_)),
                                           feeAmount,
                                           attachment,
                                           signature)
      } yield tt).fold(left => Failure(new Exception(left.toString)), right => Success(right))
    }.flatten

  def create(assetId: Option[AssetId],
             sender: PublicKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte],
             signature: ByteStr): Either[ValidationError, TransactionT] = {
    TransferTransaction
      .validate(amount, feeAmount, attachment)
      .map(_ => V1TransferTransaction(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, signature))
  }

  def create(assetId: Option[AssetId],
             sender: PrivateKeyAccount,
             recipient: AddressOrAlias,
             amount: Long,
             timestamp: Long,
             feeAssetId: Option[AssetId],
             feeAmount: Long,
             attachment: Array[Byte]): Either[ValidationError, TransactionT] = {
    create(assetId, sender, recipient, amount, timestamp, feeAssetId, feeAmount, attachment, ByteStr.empty).right.map { unsigned =>
      unsigned.copy(signature = ByteStr(crypto.sign(sender, unsigned.bodyBytes())))
    }
  }
}
