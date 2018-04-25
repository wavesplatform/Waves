package scorex.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.Json
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.modern.TxHeader
import scorex.transaction.modern.assets.{CancelFeeSponsorshipPayload, CancelFeeSponsorshipTx}
import scorex.transaction.validation.ValidationError
import scorex.transaction.validation.ValidationError.GenericError
import scorex.transaction.{AssetIdStringLength, Proofs}

object CancelFeeSponsorshipRequest {
  implicit val unsignedCancelSponsorshipRequestFormat = Json.format[CancelFeeSponsorshipRequest]
  implicit val signedCancelSponsorshipRequestFormat   = Json.format[SignedCancelFeeSponsorshipRequest]
}

case class CancelFeeSponsorshipRequest(@ApiModelProperty(required = true)
                                       version: Byte,
                                       @ApiModelProperty(value = "Sender address", required = true)
                                       sender: String,
                                       @ApiModelProperty(value = "Asset to be sponsored", required = true)
                                       assetId: String,
                                       @ApiModelProperty(required = true)
                                       fee: Long,
                                       timestamp: Option[Long] = None)

@ApiModel(value = "Signed Sponsorship Transaction")
case class SignedCancelFeeSponsorshipRequest(@ApiModelProperty(required = true)
                                             version: Byte,
                                             @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                             senderPublicKey: String,
                                             @ApiModelProperty(value = "Asset to be sponsored", required = true)
                                             assetId: String,
                                             @ApiModelProperty(required = true)
                                             fee: Long,
                                             @ApiModelProperty(required = true)
                                             timestamp: Long,
                                             @ApiModelProperty(required = true)
                                             proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CancelFeeSponsorshipTx] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- CancelFeeSponsorshipTx.create(
        TxHeader(CancelFeeSponsorshipTx.typeId, version, _sender, fee, timestamp),
        CancelFeeSponsorshipPayload(_assetId),
        _proofs
      ).toEither.left.map(thr => GenericError(thr.getMessage))
    } yield t
}
