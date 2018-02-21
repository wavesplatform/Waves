package scorex.api.http

import io.swagger.annotations.{ApiModel, ApiModelProperty}
import play.api.libs.json.{Format, Json}
import scorex.account.PublicKeyAccount
import scorex.transaction.DataTransaction.Data
import scorex.transaction.TransactionParser.SignatureStringLength
import scorex.transaction.{DataTransaction, ValidationError}

object SignedDataRequest {
  implicit val jsonFormat: Format[SignedDataRequest] = Json.format
}

@ApiModel(value = "Signed Data transaction")
case class SignedDataRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                             senderPublicKey: String,
                             @ApiModelProperty(value = "///", required = true)
                             data: Data,
                             @ApiModelProperty(required = true)
                             fee: Long,
                             @ApiModelProperty(required = true)
                             timestamp: Long,
                             @ApiModelProperty(required = true)
                             signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    t <- DataTransaction.create(_sender, data, timestamp, fee, _signature) ///data
  } yield t
}
