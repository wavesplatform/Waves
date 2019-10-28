package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{Alias, PublicKey}
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.{CreateAliasTransaction, TxAmount, TxTimestamp, TxVersion}
import play.api.libs.json.Json

case class CreateAliasRequest(
    alias: String,
    fee: TxAmount,
    version: Option[TxVersion] = None,
    timestamp: Option[TxTimestamp] = None,
    sender: Option[String] = None,
    senderPublicKey: Option[String] = None,
    signature: Option[String] = None,
    proofs: Option[List[String]] = None
) extends TxBroadcastRequest[CreateAliasTransaction] {
  def toTxFrom(sender: PublicKey): Either[ValidationError, CreateAliasTransaction] =
    for {
      validAlias  <- Alias.create(alias)
      validProofs <- toProofs(version, signature, proofs)
    } yield CreateAliasTransaction(
      version.getOrElse(1.toByte),
      timestamp.getOrElse(0L),
      sender,
      validAlias,
      fee,
      validProofs
    )
}

object CreateAliasRequest {
  implicit val jsonFormat = Json.format[CreateAliasRequest]
}
