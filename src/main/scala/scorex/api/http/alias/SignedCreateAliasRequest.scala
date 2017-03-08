package scorex.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}
import scorex.account.{Alias, PublicKeyAccount}
import scorex.api.http.BroadcastRequest
import scorex.transaction.TransactionParser.SignatureStringLength
import scorex.transaction.{CreateAliasTransaction, ValidationError}

case class SignedCreateAliasRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                    senderPublicKey: String,
                                    @ApiModelProperty(required = true)
                                    fee: Long,
                                    @ApiModelProperty(value = "Alias", required = true)
                                    alias: String,
                                    @ApiModelProperty(required = true)
                                    timestamp: Long,
                                    @ApiModelProperty(required = true)
                                    signature: String) extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateAliasTransaction] = for {
    _sender <- PublicKeyAccount.fromBase58String(senderPublicKey)
    _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
    _alias <- Alias(alias)
    _t <- CreateAliasTransaction.create(_sender, _alias, fee, timestamp, _signature)
  } yield _t
}

object SignedCreateAliasRequest {
  implicit val broadcastAliasRequestReadsFormat: Format[SignedCreateAliasRequest] = Json.format
}
