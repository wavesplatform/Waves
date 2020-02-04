package com.wavesplatform.api.http.alias

import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.CreateAliasTransactionV1
import play.api.libs.json.{Format, Json}

case class SignedCreateAliasV1Request(senderPublicKey: String,
                                      fee: Long,
                                      alias: String,
                                      timestamp: Long,
                                      signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, CreateAliasTransactionV1] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid.signature", SignatureStringLength)
      _alias     <- Alias.create(alias)
      _t         <- CreateAliasTransactionV1.create(_sender, _alias, fee, timestamp, _signature)
    } yield _t
}

object SignedCreateAliasV1Request {
  implicit val broadcastAliasV1RequestReadsFormat: Format[SignedCreateAliasV1Request] = Json.format
}
