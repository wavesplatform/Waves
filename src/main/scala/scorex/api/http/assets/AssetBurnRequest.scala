package scorex.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.formats._
import scorex.crypto.encode.Base58
import scorex.transaction.assets._

import scala.util.Try

case class AssetBurnRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                            senderPublicKey: String,
                            @ApiModelProperty(value = "Base58 encoded Asset ID", required = true)
                            assetId: String,
                            @ApiModelProperty(required = true, example = "1000000")
                            amount: Long,
                            @ApiModelProperty(required = true)
                            fee: Long,
                            @ApiModelProperty(required = true)
                            timestamp: Long,
                            @ApiModelProperty(required = true)
                            signature: String) {

  def toTx: Try[BurnTransaction] = Try {
    BurnTransaction.create(
      new PublicKeyAccount(Base58.decode(senderPublicKey).get),
      Base58.decode(assetId).get,
      amount,
      fee,
      timestamp,
      Base58.decode(signature).get).right.get
  }
}

object AssetBurnRequest {


  //TODO put reads/writes together?
  implicit val assetBurnRequestReads: Reads[AssetBurnRequest] = (
    (JsPath \ "senderPublicKey").read[String] and
      (JsPath \ "assetId").read[String] and
      (JsPath \ "quantity").read[Long] and
      (JsPath \ "fee").read[Long] and
      (JsPath \ "timestamp").read[Long] and
      (JsPath \ "signature").read[String]
    ) (AssetBurnRequest.apply _)

}
