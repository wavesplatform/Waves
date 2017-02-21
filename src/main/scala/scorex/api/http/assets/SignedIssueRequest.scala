package scorex.api.http.assets

import com.google.common.base.Charsets
import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.api.http.BroadcastRequest
import scorex.transaction.TypedTransaction.SignatureStringLength
import scorex.transaction.ValidationError
import scorex.transaction.assets.IssueTransaction


object SignedIssueRequest {
  implicit val assetIssueRequestReads: Format[SignedIssueRequest] = Json.format
}

@ApiModel(value = "Signed Asset issue transaction")
case class SignedIssueRequest(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                             senderPublicKey: String,
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
                              @ApiModelProperty(required = true)
                             fee: Long,
                              @ApiModelProperty(required = true)
                             timestamp: Long,
                              @ApiModelProperty(required = true)
                             signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, IssueTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid signature", SignatureStringLength)
    _t <- IssueTransaction.create(_sender, name.getBytes(Charsets.UTF_8), description.getBytes(Charsets.UTF_8),
      quantity, decimals, reissuable, fee, timestamp, _signature)
  } yield _t
}
