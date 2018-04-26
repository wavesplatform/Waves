package scorex.api.http.assets

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{AddressOrAlias, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import scorex.transaction.TransactionParsers.SignatureStringLength
import scorex.transaction.assets.TransferTransaction
import scorex.transaction.AssetIdStringLength
import scorex.transaction.validation.ValidationError

object SignedTransferRequest {
  implicit val reads: Reads[SignedTransferRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "recipient").read[String] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "feeAssetId").read[String].map(Option.apply).orElse((JsPath \ "feeAsset").readNullable[String]) and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "signature").read[String]
  )(SignedTransferRequest.apply _)

  implicit val writes: Writes[SignedTransferRequest] = Json.writes[SignedTransferRequest]
}

@ApiModel(value = "Signed Asset transfer transaction")
case class SignedTransferRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                 senderPublicKey: String,
                                 @ApiModelProperty(value = "Base58 encoded Asset ID")
                                 assetId: Option[String],
                                 @ApiModelProperty(value = "Recipient address", required = true)
                                 recipient: String,
                                 @ApiModelProperty(required = true, example = "1000000")
                                 amount: Long,
                                 @ApiModelProperty(required = true)
                                 fee: Long,
                                 @ApiModelProperty(value = "Fee asset ID")
                                 feeAssetId: Option[String],
                                 @ApiModelProperty(required = true)
                                 timestamp: Long,
                                 @ApiModelProperty(value = "Base58 encoded attachment")
                                 attachment: Option[String],
                                 @ApiModelProperty(required = true)
                                 signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, TransferTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(senderPublicKey)
      _assetId    <- parseBase58ToOption(assetId.filter(_.length > 0), "invalid.assetId", AssetIdStringLength)
      _feeAssetId <- parseBase58ToOption(feeAssetId.filter(_.length > 0), "invalid.feeAssetId", AssetIdStringLength)
      _signature  <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _attachment <- parseBase58(attachment.filter(_.length > 0), "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
      _account    <- AddressOrAlias.fromString(recipient)
      t           <- TransferTransaction.create(_assetId, _sender, _account, amount, timestamp, _feeAssetId, fee, _attachment.arr, _signature)
    } yield t
}
