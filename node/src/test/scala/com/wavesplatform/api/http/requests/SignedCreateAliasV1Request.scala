package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.{CreateAliasTransaction, Proofs}
import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class SignedCreateAliasV1Request(
    @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
    senderPublicKey: String,
    @ApiModelProperty(required = true)
    fee: Long,
    @ApiModelProperty(value = "Alias", required = true)
    alias: String,
    @ApiModelProperty(required = true)
    timestamp: Long,
    @ApiModelProperty(required = true)
    signature: String
) {
  def toTx: Either[ValidationError, CreateAliasTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _alias     <- Alias.create(alias)
      _t         <- CreateAliasTransaction.create(1: Byte, _sender, _alias, fee, timestamp, Proofs(_signature))
    } yield _t
}

object SignedCreateAliasV1Request {
  implicit val jsonFormat: Format[SignedCreateAliasV1Request] = Json.format
}
