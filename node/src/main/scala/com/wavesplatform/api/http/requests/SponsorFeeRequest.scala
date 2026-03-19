package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import play.api.libs.json.{Format, Json}

object SponsorFeeRequest {
  implicit val unsignedSponsorRequestFormat: Format[SponsorFeeRequest]     = Json.format
  implicit val signedSponsorRequestFormat: Format[SignedSponsorFeeRequest] = Json.format
}

case class SponsorFeeRequest(
    version: Option[Byte],
    sender: String,
    assetId: String,
    minSponsoredAssetFee: Option[Long],
    fee: Long,
    timestamp: Option[Long] = None
)

case class SignedSponsorFeeRequest(
    version: Byte = 1.toByte,
    senderPublicKey: String,
    assetId: IssuedAsset,
    minSponsoredAssetFee: Option[Long],
    fee: Long,
    timestamp: Long,
    proofs: Proofs
) extends TxBroadcastRequest[SponsorFeeTransaction] {
  def toTx: Either[ValidationError, SponsorFeeTransaction] =
    for {
      validSender <- PublicKey.fromBase58String(senderPublicKey)
      t           <- SponsorFeeTransaction.create(version, validSender, assetId, minSponsoredAssetFee.filterNot(_ == 0), fee, timestamp, proofs)
    } yield t
}
