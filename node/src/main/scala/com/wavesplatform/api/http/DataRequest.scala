package com.wavesplatform.api.http

import cats.implicits._
import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import play.api.libs.json.Json

object DataRequest {
  implicit val unsignedDataRequestReads = Json.reads[DataRequest]
  implicit val signedDataRequestReads   = Json.reads[SignedDataRequest]
}

case class DataRequest(sender: String, data: List[DataEntry[_]], fee: Long, timestamp: Option[Long] = None)

case class SignedDataRequest(senderPublicKey: String, data: List[DataEntry[_]], fee: Long, timestamp: Long, proofs: List[String])
    extends BroadcastRequest {
  def toTx: Either[ValidationError, DataTransaction] =
    for {
      _sender     <- PublicKey.fromBase58String(senderPublicKey)
      _proofBytes <- proofs.traverse(s => parseBase58(s, "invalid proof", Proofs.MaxProofStringSize))
      _proofs     <- Proofs.create(_proofBytes)
      t           <- DataTransaction.create(_sender, data, fee, timestamp, _proofs)
    } yield t
}
