package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.Proofs
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.lease.LeaseTransaction
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class SignedLeaseV1Request(
    @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
    senderPublicKey: String,
    @ApiModelProperty(required = true)
    amount: Long,
    @ApiModelProperty(required = true)
    fee: Long,
    @ApiModelProperty(value = "Recipient address", required = true)
    recipient: String,
    @ApiModelProperty(required = true)
    timestamp: Long,
    @ApiModelProperty(required = true)
    signature: String
) {
  def toTx: Either[ValidationError, LeaseTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _recipient <- AddressOrAlias.fromString(recipient)
      _t         <- LeaseTransaction.create(1.toByte, _sender, _recipient, amount, fee, timestamp, Proofs(_signature))
    } yield _t
}

object SignedLeaseV1Request {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseV1Request] = Json.format
}
