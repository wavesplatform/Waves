package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{CreateAliasTransaction, Proofs, TxTimestamp, TxVersion}
import play.api.libs.json.{Format, Json}

case class CreateAliasRequest(
    alias: String,
    version: Option[TxVersion],
    senderPublicKey: String,
    fee: Option[Long] = None,
    timestamp: Option[TxTimestamp] = None,
    signature: Option[ByteStr] = None,
    proofs: Option[Proofs] = None
) extends TxBroadcastRequest[CreateAliasTransaction] {
  def toTx: Either[ValidationError, CreateAliasTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      validSender <- PublicKey.fromBase58String(senderPublicKey)
      tx <- CreateAliasTransaction.create(version.getOrElse(1.toByte), validSender, alias, fee.getOrElse(0L), timestamp.getOrElse(0L), validProofs)
    } yield tx
}

object CreateAliasRequest {
  implicit val jsonFormat: Format[CreateAliasRequest] = Json.format
}
