package com.wavesplatform.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}
import com.wavesplatform.account.PublicKeyAccount
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.transaction._
import com.wavesplatform.crypto._

trait BurnTransaction extends ProvenTransaction with VersionedTransaction {

  def chainByte: Option[Byte]

  def assetId: ByteStr

  def quantity: Long

  def fee: Long

  def timestamp: Long

  override val assetFee: (Option[AssetId], Long) = (None, fee)

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "chainId" -> chainByte,
      "version" -> version,
      "assetId" -> assetId.base58,
      "amount"  -> quantity,
      "fee"     -> fee
    )
  }

  val byteBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender.publicKey,
      assetId.arr,
      Longs.toByteArray(quantity),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
  override def checkedAssets(): Seq[AssetId] = Seq(assetId)
}

object BurnTransaction {

  val typeId: Byte = 6

  def parseBase(start: Int, bytes: Array[Byte]): (PublicKeyAccount, AssetId, Long, Long, Long, Int) = {
    val sender        = PublicKeyAccount(bytes.slice(start, start + KeyLength))
    val assetId       = ByteStr(bytes.slice(start + KeyLength, start + KeyLength + AssetIdLength))
    val quantityStart = start + KeyLength + AssetIdLength

    val quantity  = Longs.fromByteArray(bytes.slice(quantityStart, quantityStart + 8))
    val fee       = Longs.fromByteArray(bytes.slice(quantityStart + 8, quantityStart + 16))
    val timestamp = Longs.fromByteArray(bytes.slice(quantityStart + 16, quantityStart + 24))

    (sender, assetId, quantity, fee, timestamp, quantityStart + 24)
  }

  def validateBurnParams(amount: Long, fee: Long): Either[ValidationError, Unit] =
    if (amount < 0) {
      Left(ValidationError.NegativeAmount(amount, "assets"))
    } else if (fee <= 0) {
      Left(ValidationError.InsufficientFee())
    } else Right(())
}
