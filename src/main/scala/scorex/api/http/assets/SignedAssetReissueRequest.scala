package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.Base58Parser
import scorex.transaction.ValidationError
import scorex.transaction.assets.ReissueTransaction


case class SignedAssetReissueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                     senderPublicKey: String,
                                     @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                                     assetId: String,
                                     @ApiModelProperty(required = true, example = "1000000")
                                     quantity: Long,
                                     @ApiModelProperty(required = true)
                                     reissuable: Boolean,
                                     @ApiModelProperty(required = true)
                                     fee: Long,
                                     @ApiModelProperty(required = true)
                                     timestamp: Long,
                                     @ApiModelProperty(required = true)
                                     signature: String) {

  def toTx: Either[ValidationError, ReissueTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- Base58Parser.parseBase58(signature, "invalid.signature")
    _assetId <- Base58Parser.parseBase58(assetId, "invalid.assetId")
    _t <- ReissueTransaction.create(_sender, _assetId, quantity, reissuable, fee, timestamp, _signature)
  } yield _t
}


object SignedAssetReissueRequest {
  implicit val assetReissueRequestReads: Format[SignedAssetReissueRequest] = Json.format

}