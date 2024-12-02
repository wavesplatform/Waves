package com.wavesplatform.api.http.requests

import cats.instances.option.*
import cats.syntax.traverse.*
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.{IssuedAsset, Waves}
import com.wavesplatform.transaction.assets.UpdateAssetInfoTransaction
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs, TxTimestamp, TxVersion}
import play.api.libs.json.{Format, Json}

case class SignedUpdateAssetInfoRequest(
    version: TxVersion,
    chainId: Byte,
    senderPublicKey: String,
    assetId: IssuedAsset,
    name: String,
    description: String,
    timestamp: TxTimestamp,
    fee: Long,
    feeAssetId: Option[String],
    proofs: Proofs
) {

  def toTx: Either[ValidationError, UpdateAssetInfoTransaction] =
    for {
      _sender <- PublicKey.fromBase58String(senderPublicKey)
      _feeAssetId <- feeAssetId
        .traverse(parseBase58(_, "invalid.assetId", AssetIdStringLength).map(IssuedAsset(_)))
        .map(_ getOrElse Waves)
      tx <- UpdateAssetInfoTransaction
        .create(version, _sender, assetId.id, name, description, timestamp, fee, _feeAssetId, proofs, chainId)
    } yield tx

}

object SignedUpdateAssetInfoRequest {
  implicit val format: Format[SignedUpdateAssetInfoRequest] = Json.format
}
