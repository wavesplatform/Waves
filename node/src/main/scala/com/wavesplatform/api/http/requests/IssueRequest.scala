package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.lang.script.Script
import com.wavesplatform.transaction.assets.IssueTransaction
import com.wavesplatform.transaction.{Proofs, TxVersion}
import play.api.libs.json.{Format, Json}

case class IssueRequest(
    version: Byte = TxVersion.V3,
    senderPublicKey: String,
    name: String,
    description: String,
    quantity: Long,
    decimals: Byte,
    reissuable: Boolean,
    script: Option[String],
    fee: Long,
    timestamp: Option[Long],
    signature: Option[ByteStr],
    proofs: Option[Proofs],
    chainId: Byte = AddressScheme.current.chainId
) extends TxBroadcastRequest[IssueTransaction] {
  def toTx: Either[ValidationError, IssueTransaction] = {
    for {
      validProofs <- toProofs(signature, proofs)
      validSender <- PublicKey.fromBase58String(senderPublicKey)
      validScript <- script match {
        case None         => Right(None)
        case Some(script) => Script.fromBase64String(script).map(Some(_))
      }
      tx <- IssueTransaction.create(
        version,
        validSender,
        name,
        description,
        quantity,
        decimals,
        reissuable,
        validScript,
        fee,
        timestamp.getOrElse(defaultTimestamp),
        validProofs,
        chainId
      )
    } yield tx
  }
}

object IssueRequest {
  given Format[IssueRequest] = Json.format
}
