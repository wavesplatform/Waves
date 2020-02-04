package com.wavesplatform.api.http.assets

import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs}
import play.api.libs.json.Json

object SponsorFeeRequest {
  implicit val unsignedSponsorRequestFormat = Json.format[SponsorFeeRequest]
  implicit val signedSponsorRequestFormat   = Json.format[SignedSponsorFeeRequest]
}

case class SponsorFeeRequest(sender: String, assetId: String, minSponsoredAssetFee: Option[Long], fee: Long, timestamp: Option[Long] = None)

case class SignedSponsorFeeRequest(
    senderPublicKey: String,
    assetId: String,
    minSponsoredAssetFee: Option[Long],
    fee: Long,
    timestamp: Long,
    proofs: List[String]
) extends BroadcastRequest {
  def toTx: Either[ValidationError, SponsorFeeTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _asset      <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength).map(IssuedAsset)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- SponsorFeeTransaction.create(_sender, _asset, minSponsoredAssetFee, fee, timestamp, _proofs)
    } yield t
}
