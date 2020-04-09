package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{CreateAliasTransaction, Proofs}
import play.api.libs.json.{Format, Json}

case class SignedCreateAliasV1Request(
    senderPublicKey: String,
    fee: Long,
    alias: String,
    timestamp: Long,
    signature: String
) {
  def toTx: Either[ValidationError, CreateAliasTransaction] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _t         <- CreateAliasTransaction.create(1: Byte, _sender, alias, fee, timestamp, Proofs(_signature))
    } yield _t
}

object SignedCreateAliasV1Request {
  implicit val jsonFormat: Format[SignedCreateAliasV1Request] = Json.format
}
