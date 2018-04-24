package scorex.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.assets.SponsorFeeTransaction
import scorex.transaction.{AssetIdStringLength, Proofs, ValidationError}

object SponsorFeeRequest {
  implicit val unsignedSponsorRequestFormat = Json.format[SponsorFeeRequest]
  implicit val signedSponsorRequestFormat   = Json.format[SignedSponsorFeeRequest]
}

case class SponsorFeeRequest(@ApiModelProperty(required = true)
                             version: Byte,
                             @ApiModelProperty(value = "Sender address", required = true)
                             sender: String,
                             @ApiModelProperty(value = "Asset to be sponsored", required = true)
                             assetId: String,
                             @ApiModelProperty(value = "Asset amount per fee unit", required = true)
                             baseFee: Long,
                             @ApiModelProperty(required = true)
                             fee: Long,
                             timestamp: Option[Long] = None)

@ApiModel(value = "Signed Sponsorship Transaction")
case class SignedSponsorFeeRequest(@ApiModelProperty(required = true)
                                   version: Byte,
                                   @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                   senderPublicKey: String,
                                   @ApiModelProperty(value = "Asset to be sponsored", required = true)
                                   assetId: String,
                                   @ApiModelProperty(required = true)
                                   baseFee: Long,
                                   @ApiModelProperty(required = true)
                                   fee: Long,
                                   @ApiModelProperty(required = true)
                                   timestamp: Long,
                                   @ApiModelProperty(required = true)
                                   proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, SponsorFeeTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- SponsorFeeTransaction.create(version, _sender, _assetId, baseFee, fee, timestamp, _proofs)
    } yield t
}
