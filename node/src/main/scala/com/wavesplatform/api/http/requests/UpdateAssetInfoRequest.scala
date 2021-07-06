package com.wavesplatform.api.http.requests

import cats.instances.option._
import cats.syntax.traverse._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs, Transaction, TxAmount, TxTimestamp, TxVersion}
import play.api.libs.json.Json

case class UpdateAssetInfoRequest(
    version: TxVersion,
    chainId: Byte,
    sender: Option[String],
    senderPublicKey: Option[String],
    assetId: String,
    name: String,
    description: String,
    timestamp: Option[TxTimestamp],
    fee: TxAmount,
    feeAssetId: Option[String],
    proofs: Option[Proofs]
) extends TxBroadcastRequest {
  override def toTxFrom(sender: PublicKey): Either[ValidationError, Transaction] =
    for {
      _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _feeAssetId <- feeAssetId
        .traverse(parseBase58(_, "invalid.assetId", AssetIdStringLength).map(IssuedAsset))
        .map(_ getOrElse Waves)
      tx <- UpdateAssetInfoTransaction
        .create(version, sender, _assetId, name, description, timestamp.getOrElse(0L), fee, _feeAssetId, proofs.getOrElse(Proofs.empty), chainId)
    } yield tx
}

object UpdateAssetInfoRequest {
  implicit val jsonFormat = Json.format[UpdateAssetInfoRequest]
}
