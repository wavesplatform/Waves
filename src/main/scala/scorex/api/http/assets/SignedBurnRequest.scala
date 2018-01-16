package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.TransactionParser.SignatureStringLength
import scorex.transaction.assets.BurnTransaction
import scorex.transaction.{AssetIdStringLength, ValidationError}

object SignedBurnRequest {
  implicit val reads: Reads[SignedBurnRequest] = (
      (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long].orElse((JsPath \ "amount").read[Long]) and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String]
    )(SignedBurnRequest.apply _)

  implicit val writes: Writes[SignedBurnRequest] = Json.writes[SignedBurnRequest]
}

case class SignedBurnRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                            senderPublicKey: String,
                             @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                            assetId: String,
                             @ApiModelProperty(required = true, example = "1000000")
                            quantity: Long,
                             @ApiModelProperty(required = true)
                            fee: Long,
                             @ApiModelProperty(required = true)
                            timestamp: Long,
                             @ApiModelProperty(required = true)
                            signature: String) extends BroadcastRequest {

  def toTx: Either[ValidationError, BurnTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _assetId <- parseBase58(assetId, "invalid.assetId", AssetIdStringLength)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _t <- BurnTransaction.create(_sender, _assetId, quantity, fee, timestamp, _signature)
  } yield _t
}
