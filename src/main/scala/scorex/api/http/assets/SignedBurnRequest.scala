package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.TypedTransaction.SignatureStringLength
import scorex.transaction.{AssetIdStringLength, ValidationError}
import scorex.transaction.assets.BurnTransaction

object SignedBurnRequest {
  implicit val assetBurnRequestReads: Format[SignedBurnRequest] = Json.format
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
