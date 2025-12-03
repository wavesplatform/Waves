package com.wavesplatform.api.http.requests

import cats.instances.option.*
import cats.syntax.traverse.*
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs, TxTimestamp, TxVersion}
import play.api.libs.json.{Json, OFormat}

case class UpdateAssetInfoRequest(
    version: TxVersion,
    chainId: Byte,
    sender: Option[String],
    senderPublicKey: Option[String],
    assetId: String,
    name: String,
    description: String,
    timestamp: Option[TxTimestamp],
    fee: Long,
    feeAssetId: Option[String],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[UpdateAssetInfoTransaction] {
  override def toTxFrom(sender: PublicKey): Either[ValidationError, UpdateAssetInfoTransaction] =
    for {
      _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _feeAssetId <- feeAssetId
        .traverse(parseBase58(_, "invalid.assetId", AssetIdStringLength).map(IssuedAsset(_)))
        .map(_ getOrElse Waves)
      tx <- UpdateAssetInfoTransaction
        .create(version, sender, _assetId, name, description, timestamp.getOrElse(0L), fee, _feeAssetId, proofs.getOrElse(Proofs.empty), chainId)
    } yield tx
}

object UpdateAssetInfoRequest {
  implicit val jsonFormat: OFormat[UpdateAssetInfoRequest] = Json.format[UpdateAssetInfoRequest]
}
