package com.wavesplatform.api.http.leasing

import cats.implicits._
import com.wavesplatform.account.{AddressOrAlias, PublicKey}
import com.wavesplatform.api.http.BroadcastRequest
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.transaction.lease.LeaseTransactionV2
import com.wavesplatform.transaction.Proofs
import play.api.libs.json.{Format, Json}

case class SignedLeaseV2Request(senderPublicKey: String,
                                amount: Long,
                                fee: Long,
                                recipient: String,
                                timestamp: Long,
                                proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, LeaseTransactionV2] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      _recipient  <- AddressOrAlias.fromString(recipient)
      _t          <- LeaseTransactionV2.create(_sender, amount, fee, timestamp, _recipient, _proofs)
    } yield _t
}

object SignedLeaseV2Request {
  implicit val broadcastLeaseRequestReadsFormat: Format[SignedLeaseV2Request] = Json.format
}
