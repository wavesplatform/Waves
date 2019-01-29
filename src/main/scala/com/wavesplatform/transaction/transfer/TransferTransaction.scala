package com.wavesplatform.transaction.transfer

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.account.{AddressOrAlias, PublicKeyAccount}
import com.wavesplatform.common.utils.Base58
import com.wavesplatform.serialization.Deser
import com.wavesplatform.transaction._
import com.wavesplatform.transaction.validation._
import com.wavesplatform.utils.base58Length
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import com.wavesplatform.crypto._

trait TransferTransaction extends ProvenTransaction with VersionedTransaction {
  def assetId: Option[AssetId]
  def recipient: AddressOrAlias
  def amount: Long
  def feeAssetId: Option[AssetId]
  def fee: Long
  def attachment: Array[Byte]
  def version: Byte

  override val assetFee: (Option[AssetId], Long) = (feeAssetId, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "recipient"  -> recipient.stringRepr,
      "assetId"    -> assetId.map(_.base58),
      "feeAssetId" -> feeAssetId.map(_.base58),
      "feeAsset"   -> feeAssetId.map(_.base58), // legacy v0.11.1 compat
      "amount"     -> amount,
      "attachment" -> Base58.encode(attachment)
    ))

  final protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    val timestampBytes  = Longs.toByteArray(timestamp)
    val assetIdBytes    = assetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val feeAssetIdBytes = feeAssetId.map(a => (1: Byte) +: a.arr).getOrElse(Array(0: Byte))
    val amountBytes     = Longs.toByteArray(amount)
    val feeBytes        = Longs.toByteArray(fee)

    Bytes.concat(
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
  override def checkedAssets(): Seq[AssetId] = assetId.toSeq
}

object TransferTransaction {

  val typeId: Byte = 4

  val MaxAttachmentSize            = 140
  val MaxAttachmentStringSize: Int = base58Length(MaxAttachmentSize)

  def validate(amount: Long, feeAmount: Long, attachment: Array[Byte]): Either[ValidationError, Unit] = {
    (
      validateAmount(amount, "waves"),
      validateFee(feeAmount),
      validateAttachment(attachment),
      validateSum(Seq(amount, feeAmount))
    ).mapN { case _ => () }
      .toEither
      .leftMap(_.head)
  }

  def parseBase(bytes: Array[Byte], start: Int) = {
    val sender              = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val (assetIdOpt, s0)    = Deser.parseByteArrayOption(bytes, start + KeyLength, AssetIdLength)
    val (feeAssetIdOpt, s1) = Deser.parseByteArrayOption(bytes, s0, AssetIdLength)
    val timestamp           = Longs.fromByteArray(bytes.slice(s1, s1 + 8))
    val amount              = Longs.fromByteArray(bytes.slice(s1 + 8, s1 + 16))
    val feeAmount           = Longs.fromByteArray(bytes.slice(s1 + 16, s1 + 24))
    for {
      recRes <- AddressOrAlias.fromBytes(bytes, s1 + 24)
      (recipient, recipientEnd) = recRes
      (attachment, end)         = Deser.parseArraySize(bytes, recipientEnd)
    } yield (sender, assetIdOpt, feeAssetIdOpt, timestamp, amount, feeAmount, recipient, attachment, end)

  }

}
