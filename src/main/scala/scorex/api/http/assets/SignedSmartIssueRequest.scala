package scorex.api.http.assets

import cats.implicits._
import com.google.common.base.Charsets
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import scorex.account.{AddressScheme, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import scorex.transaction.{Proofs, ValidationError}
import scorex.transaction.assets.{SmartIssueTransaction, VersionedTransferTransaction}
import scorex.transaction.smart.Script
import scorex.transaction.assets.exchange.OrderJson.orderFormat

object SignedSmartIssueRequest {
  implicit val signedExchangeRequestFormat: Format[SignedSmartIssueRequest] = Json.format
}

@ApiModel(value = "Signed Smart issue transaction")
case class SignedSmartIssueRequest(@ApiModelProperty(required = true)
                                   version: Int,
                                   @ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                                   sender: String,
                                   @ApiModelProperty(value = "Base58 encoded name of Asset", required = true)
                                   name: String,
                                   @ApiModelProperty(value = "Base58 encoded description of Asset", required = true)
                                   description: String,
                                   @ApiModelProperty(required = true, example = "1000000")
                                   quantity: Long,
                                   @ApiModelProperty(allowableValues = "range[0,8]", example = "8", dataType = "integer", required = true)
                                   decimals: Byte,
                                   @ApiModelProperty(required = true)
                                   reissuable: Boolean,
                                   script: Option[String],
                                   @ApiModelProperty(required = true)
                                   fee: Long,
                                   @ApiModelProperty(required = true)
                                   timestamp: Long,
                                   @ApiModelProperty(required = true)
                                   proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, SmartIssueTransaction] =
    for {
      _sender     <- PublicKeyAccount.fromBase58String(sender)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _script <- script match {
        case None    => Right(None)
        case Some(s) => Script.fromBase58String(s).map(Some(_))
      }
      t <- SmartIssueTransaction.create(
        version.toByte,
        AddressScheme.current.chainId,
        _sender,
        name.getBytes(Charsets.UTF_8),
        description.getBytes(Charsets.UTF_8),
        quantity,
        decimals,
        reissuable,
        _script,
        fee,
        timestamp,
        _proofs
      )
    } yield t
}
