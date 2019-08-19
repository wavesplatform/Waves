package com.wavesplatform.transaction.assets

import cats.implicits._
import com.google.common.primitives.{Bytes, Longs}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.validation._
import com.wavesplatform.transaction.{Asset, ProvenTransaction, _}
import monix.eval.Coeval
import play.api.libs.json.{JsObject, Json}

trait ReissueTransaction extends ProvenTransaction with VersionedTransaction {
  def asset: IssuedAsset
  def quantity: Long
  def reissuable: Boolean
  def fee: Long

  override val assetFee: (Asset, Long) = (Waves, fee)

  override final val json: Coeval[JsObject] = Coeval.evalOnce(
    jsonBase() ++ Json.obj(
      "version"    -> version,
      "chainId"    -> chainByte,
      "assetId"    -> asset.id.base58,
      "quantity"   -> quantity,
      "reissuable" -> reissuable
    ))

  protected val bytesBase: Coeval[Array[Byte]] = Coeval.evalOnce {
    Bytes.concat(
      sender,
      asset.id.arr,
      Longs.toByteArray(quantity),
      if (reissuable) Array(1: Byte) else Array(0: Byte),
      Longs.toByteArray(fee),
      Longs.toByteArray(timestamp)
    )
  }
  override def checkedAssets(): Seq[IssuedAsset] = Seq(asset)
}

object ReissueTransaction {

  val typeId: Byte = 5

  def validateReissueParams(tx: ReissueTransaction): Either[ValidationError, Unit] = {
    validateReissueParams(tx.quantity, tx.fee)
  }

  def validateReissueParams(quantity: Long, fee: Long): Either[ValidationError, Unit] =
    (validateAmount(quantity, "assets"), validateFee(fee))
      .mapN { case _ => () }
      .leftMap(_.head)
      .toEither
}
