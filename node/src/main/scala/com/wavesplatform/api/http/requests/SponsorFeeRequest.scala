package com.wavesplatform.api.http.requests

import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Asset.IssuedAsset
import com.wavesplatform.transaction.assets.SponsorFeeTransaction
import com.wavesplatform.transaction.{AssetIdStringLength, Proofs}
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json

object SponsorFeeRequest {
  implicit val unsignedSponsorRequestFormat = Json.format[SponsorFeeRequest]
  implicit val signedSponsorRequestFormat   = Json.format[SignedSponsorFeeRequest]
}

case class SponsorFeeRequest(
    @ApiModelProperty(value = "Sender address", required = true)
    sender: String,
    @ApiModelProperty(value = "Asset to be sponsored", required = true)
    assetId: String,
    @ApiModelProperty(value = "Asset amount per fee unit", required = true)
    minSponsoredAssetFee: Option[Long],
    @ApiModelProperty(required = true)
    fee: Long,
    timestamp: Option[Long] = None
)

@ApiModel(value = "Signed Sponsorship Transaction")
case class SignedSponsorFeeRequest(
    @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
    senderPublicKey: String,
    @ApiModelProperty(value = "Asset to be sponsored", required = true)
    assetId: String,
    @ApiModelProperty(required = true)
    minSponsoredAssetFee: Option[Long],
    @ApiModelProperty(required = true)
    fee: Long,
    @ApiModelProperty(required = true)
    timestamp: Long,
    @ApiModelProperty(required = true)
    proofs: List[String]
) {
  def toTx: Either[ValidationError, SponsorFeeTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _asset      <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength).map(IssuedAsset)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- SponsorFeeTransaction.create(1.toByte, _sender, _asset, minSponsoredAssetFee.filterNot(_ == 0), fee, timestamp, _proofs)
    } yield t
}
