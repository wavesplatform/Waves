package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{Format, Json}

case class IssueRequest(
    version: Option[Byte],
    sender: Option[String],
    senderPublicKey: Option[String],
    name: String,
    description: String,
    quantity: Long,
    decimals: Byte,
    reissuable: Boolean,
    script: Option[String],
    fee: Long,
    timestamp: Option[Long],
    signature: Option[ByteStr],
    proofs: Option[Proofs]
) extends TxBroadcastRequest[IssueTransaction] {
  def toTxFrom(sender: PublicKey): Either[ValidationError, IssueTransaction] = {
    val actualVersion = version.getOrElse(TxVersion.V3)

    for {
      validProofs <- toProofs(signature, proofs)
      validScript <- script match {
        case None         => Right(None)
        case Some(script) => Script.fromBase64String(script).map(Some(_))
      }
      tx <- IssueTransaction.create(
        actualVersion,
        sender,
        name,
        description,
        quantity,
        decimals,
        reissuable,
        validScript,
        fee,
        timestamp.getOrElse(defaultTimestamp),
        validProofs
      )
    } yield tx
  }
}

object IssueRequest {
  implicit val jsonFormat: Format[IssueRequest] = Json.format
}
