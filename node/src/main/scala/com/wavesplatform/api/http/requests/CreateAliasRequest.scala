package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{CreateAliasTransaction, TxAmount, TxTimestamp, TxVersion}
import play.api.libs.json.Json

case class CreateAliasRequest(
    alias: String,
    version: Option[TxVersion] = None,
    sender: Option[String] = None,
    senderPublicKey: Option[String] = None,
    fee: Option[TxAmount] = None,
    timestamp: Option[TxTimestamp] = None,
    signature: Option[String] = None,
    proofs: Option[List[String]] = None
) extends TxBroadcastRequest {
  def toTxFrom(sender: PublicKey): Either[ValidationError, CreateAliasTransaction] =
    for {
      validAlias  <- Alias.create(alias)
      validProofs <- toProofs(version, signature, proofs)
      tx          <- CreateAliasTransaction.create(version.getOrElse(1.toByte), sender, validAlias, fee.getOrElse(0L), timestamp.getOrElse(0L), validProofs)
    } yield tx
}

object CreateAliasRequest {
  implicit val jsonFormat = Json.format[CreateAliasRequest]
}
