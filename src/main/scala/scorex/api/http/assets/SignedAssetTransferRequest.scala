package scorex.api.http.assets

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import scorex.account.{Account, PublicKeyAccount}
import scorex.crypto.encode.Base58
import scorex.transaction.assets.TransferTransaction
import play.api.libs.functional.syntax._
import play.api.libs.json.{JsPath, Reads}

import scala.util.Try
import scorex.api.http.formats._

@ApiModel(value = "Signed Asset transfer transaction")
case class SignedAssetTransferRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                sender: PublicKeyAccount,
                                      @ApiModelProperty(value = "Base58 encoded Asset ID")
                                assetId: Option[String],
                                      @ApiModelProperty(value = "Recipient address", required = true)
                                recipient: Account,
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
  def toTx: Try[TransferTransaction] = Try {
    TransferTransaction.create(
      assetId.map(Base58.decode(_).get),
      sender,
      recipient,
      amount,
      timestamp,
      feeAssetId.map(_.getBytes),
      fee,
      attachment.filter(_.nonEmpty).map(Base58.decode(_).get).getOrElse(Array.emptyByteArray),
      Base58.decode(signature).get).right.get
  }
}


object SignedAssetTransferRequest {

  implicit val assetTransferRequestReads: Reads[SignedAssetTransferRequest] = (
    (JsPath \ "senderPublicKey").read[PublicKeyAccount] and
      (JsPath \ "assetId").readNullable[String] and
      (JsPath \ "recipient").read[Account] and
      (JsPath \ "amount").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "feeAssetId").readNullable[String] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "attachment").readNullable[String] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (SignedAssetTransferRequest.apply _)
}