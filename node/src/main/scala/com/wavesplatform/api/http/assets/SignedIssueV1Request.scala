package com.wavesplatform.api.http.assets

import com.google.common.base.Charsets
import com.wavesplatform.account.PublicKey
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.TransactionParsers.SignatureStringLength
import com.wavesplatform.transaction.assets.IssueTransactionV1
import play.api.libs.json.{Format, Json}

object SignedIssueV1Request {
  implicit val assetIssueRequestReads: Format[SignedIssueV1Request] = Json.format
}

case class SignedIssueV1Request(senderPublicKey: String,
                                name: String,
                                description: String,
                                quantity: Long,
                                decimals: Byte,
                                reissuable: Boolean,
                                fee: Long,
                                timestamp: Long,
                                signature: String)
    extends BroadcastRequest {
  def toTx: Either[ValidationError, IssueTransactionV1] =
    for {
      _sender    <- PublicKey.fromBase58String(senderPublicKey)
      _signature <- parseBase58(signature, "invalid signature", SignatureStringLength)
      _t <- IssueTransactionV1.create(_sender,
                                      name.getBytes(Charsets.UTF_8),
                                      description.getBytes(Charsets.UTF_8),
                                      quantity,
                                      decimals,
                                      reissuable,
                                      fee,
                                      timestamp,
                                      _signature)
    } yield _t
}
