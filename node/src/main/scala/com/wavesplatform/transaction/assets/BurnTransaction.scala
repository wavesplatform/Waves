package com.wavesplatform.transaction.assets

import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.TxValidationError._
import com.wavesplatform.transaction._
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait BurnTransaction extends ProvenTransaction with VersionedTransaction {

  def chainByte: Option[Byte]

  def asset: IssuedAsset

  def quantity: Long

  def fee: Long

  def timestamp: Long

  override val assetFee: (Asset, Long) = (Waves, fee)

  override val json: Coeval[JsObject] = Coeval.evalOnce {
    jsonBase() ++ Json.obj(
      "version" -> version,
      "assetId" -> asset.id.base58,
      "amount"  -> quantity,
      "fee"     -> fee
    ) ++ (chainByte match {
      case Some(chainByte) => Json.obj("chainId" -> chainByte)
      case None            => JsObject.empty
    })
  }

  val byteBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender,
      asset.id.arr,
      Longs.toByteArray(quantity),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
  override def checkedAssets(): Seq[IssuedAsset] = Seq(asset)
}

object BurnTransaction {

  val typeId: Byte = 6

  def validateBurnParams(tx: BurnTransaction): Either[ValidationError, Unit] = {
    validateBurnParams(tx.quantity, tx.fee)
  }

  def validateBurnParams(amount: Long, fee: Long): Either[ValidationError, Unit] =
    if (amount < 0) {
      Left(NegativeAmount(amount, "assets"))
    } else if (fee <= 0) {
      Left(InsufficientFee())
    } else Right(())
}
