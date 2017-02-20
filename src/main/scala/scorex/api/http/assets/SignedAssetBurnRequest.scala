package scorex.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import scorex.account.{Account, PublicKeyAccount}
import scorex.api.http.Base58Parser
import scorex.api.http.formats._
import scorex.crypto.encode.Base58
import scorex.transaction.ValidationError
import scorex.transaction.assets._

import scala.util.Try

case class SignedAssetBurnRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
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
                                  signature: String) {

  def toTx: Either[ValidationError, BurnTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _assetId <- Base58Parser.parseBase58(assetId, "invalid.signature")
    _signature <- Base58Parser.parseBase58(signature, "invalid.signature")
    _t <- BurnTransaction.create(_sender, _assetId, quantity, fee, timestamp, _signature)
  } yield _t
}

object SignedAssetBurnRequest {

  implicit val assetBurnRequestReads: Format[SignedAssetBurnRequest] = Json.format


}
