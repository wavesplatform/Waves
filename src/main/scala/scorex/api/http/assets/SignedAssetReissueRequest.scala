package scorex.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{JsPath, Reads}
import scorex.account.PublicKeyAccount
import scorex.api.http.formats.SignatureReads
import scorex.crypto.encode.Base58
import scorex.transaction.assets.ReissueTransaction
import play.api.libs.functional.syntax._
import scala.util.Try
import scorex.api.http.formats._


case class SignedAssetReissueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                     senderPublicKey: PublicKeyAccount,
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

  def toTx: Try[ReissueTransaction] = Try {
    ReissueTransaction.create(
      senderPublicKey,
      Base58.decode(assetId).get,
      quantity,
      reissuable,
      fee,
      timestamp,
      Base58.decode(signature).get).right.get
  }
}

object SignedAssetReissueRequest {

  implicit val assetReissueRequestReads: Reads[SignedAssetReissueRequest] = (
    (JsPath \ "senderPublicKey").read[PublicKeyAccount] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "reissuable").read[Boolean] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String](SignatureReads)
    ) (SignedAssetReissueRequest.apply _)
}