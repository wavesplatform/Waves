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
    senderPublicKey: String,
    assetId: IssuedAsset,
    name: String,
    description: String,
    timestamp: Option[TxTimestamp],
    fee: Long,
    feeAssetId: Option[String],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[UpdateAssetInfoTransaction] {
  override def toTx: Either[ValidationError, UpdateAssetInfoTransaction] =
    for {
      _feeAssetId <- feeAssetId
        .traverse(parseBase58(_, "invalid.assetId", AssetIdStringLength).map(IssuedAsset(_)))
        .map(_ getOrElse Waves)
      _sender <- PublicKey.fromBase58String(senderPublicKey)
      tx <- UpdateAssetInfoTransaction
        .create(version, _sender, assetId, name, description, timestamp.getOrElse(0L), fee, _feeAssetId, proofs.getOrElse(Proofs.empty), chainId)
    } yield tx
}

object UpdateAssetInfoRequest {
  given OFormat[UpdateAssetInfoRequest] = Json.format[UpdateAssetInfoRequest]
}
