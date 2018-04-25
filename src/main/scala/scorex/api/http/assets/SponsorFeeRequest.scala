package scorex.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.modern.TxHeader
import scorex.transaction.modern.assets.{SponsorFeePayload, SponsorFeeTx}
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.GenericError
import scorex.transaction.{AssetIdStringLength, Proofs}

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
  def toTx: Either[ValidationError, SponsorFeeTx] =
    for {
      _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
      header = TxHeader(SponsorFeeTx.typeId, version, _sender, fee, timestamp)
      _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      payload = SponsorFeePayload(_assetId, baseFee)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t <- SponsorFeeTx
        .create(header, payload, _proofs)
        .toEither
        .left
        .map(thr => GenericError(thr.getMessage))
    } yield t
}
