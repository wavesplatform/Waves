package scorex.api.http.assets

import cats.implicits._
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.assets.MassTransferTransaction.Transfer
import scorex.transaction.assets.{MassTransferTransaction, TransferTransaction}
import scorex.transaction.{AssetIdStringLength, Proofs, ValidationError}

object SignedMassTransferRequest {
  implicit val reads: Reads[SignedMassTransferRequest] = (
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "transfers").read[List[Transfer]] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "proofs").read[List[String]].orElse((JsPath \ "signature").read[String].map(List(_)))
    )(SignedMassTransferRequest.apply _)

  implicit val writes: Writes[SignedMassTransferRequest] = Json.writes[SignedMassTransferRequest]
}

@ApiModel(value = "Signed Asset transfer transaction")
case class SignedMassTransferRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                     senderPublicKey: String,
                                     @ApiModelProperty(value = "Base58 encoded Asset ID")
                                     assetId: Option[String],
                                     @ApiModelProperty(value = "List of (recipient, amount) pairs", required = true)
                                     transfers: List[Transfer],
                                     @ApiModelProperty(required = true)
                                     fee: Long,
                                     @ApiModelProperty(required = true)
                                     timestamp: Long,
                                     @ApiModelProperty(value = "Base58 encoded attachment")
                                     attachment: Option[String],
                                     @ApiModelProperty(required = true)
                                     proofs: List[String]) extends BroadcastRequest {
  def toTx: Either[ValidationError, MassTransferTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _assetId <- parseBase58ToOption(assetId.filter(_.length > 0), "invalid.assetId", AssetIdStringLength)
    _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
    _proofs <- Proofs.create(_proofBytes)
    _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
    _transfers <- MassTransferTransaction.parseTransfersList(transfers)
    t <- MassTransferTransaction.create(Proofs.Version, _assetId, _sender, _transfers, timestamp, fee, _attachment.arr, _proofs)
  } yield t
}
