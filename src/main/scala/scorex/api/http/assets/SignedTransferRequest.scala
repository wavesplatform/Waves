package scorex.api.http.assets

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import scorex.transaction.TypedTransaction.SignatureStringLength
import scorex.transaction.{AssetIdStringLength, ValidationError}
import scorex.transaction.ValidationError.InvalidAddress
import scorex.transaction.assets.TransferTransaction

object SignedTransferRequest {
  implicit val assetTransferRequestFormat: Format[SignedTransferRequest] = Json.format
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
                                signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, TransferTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _assetId <- parseBase58ToOption(assetId, "invalid.assetId", AssetIdStringLength)
    _feeAssetId <- parseBase58ToOption(feeAssetId, "invalid.feeAssetId", AssetIdStringLength)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _attachment <- parseBase58(attachment, "invalid.attachment", TransferTransaction.MaxAttachmentStringSize)
    _account <- if (Account.isValidAddress(recipient)) Right(new Account(recipient)) else Left(InvalidAddress)
    _t <- TransferTransaction.create(_assetId, _sender, _account, amount, timestamp, _feeAssetId, fee, _attachment,
      _signature)
  } yield _t
}
