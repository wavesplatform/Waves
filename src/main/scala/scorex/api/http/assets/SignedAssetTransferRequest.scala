package scorex.api.http.assets

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.TransferTransaction
import play.api.libs.functional.syntax._
import play.api.libs.json.{Format, JsPath, Json, Reads}
import scorex.api.http.Base58Parser

import scala.util.Try
import scorex.api.http.formats._
import scorex.transaction.ValidationError
import scorex.transaction.ValidationError.InvalidAddress

@ApiModel(value = "Signed Asset transfer transaction")
case class SignedAssetTransferRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
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
                                      signature: String) {
  def toTx: Either[ValidationError, TransferTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _assetId <- Base58Parser.parseBase58ToOption(assetId, "invalid.assetId")
    _feeAssetId <- Base58Parser.parseBase58ToOption(feeAssetId, "invalid.feeAssetId")
    _signature <- Base58Parser.parseBase58(signature, "invalid.signature")
    _attachment <- Base58Parser.parseBase58(attachment, "invalid.attachment")
    _account <- if (Account.isValidAddress(recipient)) Right(new Account(recipient)) else Left(InvalidAddress)
    _t <- TransferTransaction.create(_assetId, _sender, _account, amount, timestamp, _feeAssetId, fee, _attachment,
      _signature)
  } yield _t
}


object SignedAssetTransferRequest {
  implicit val assetTransferRequestFormat: Format[SignedAssetTransferRequest] = Json.format

}