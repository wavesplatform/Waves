package com.wavesplatform.api.http.requests

import com.wavesplatform.account.PublicKey
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import play.api.libs.json.{Format, Json}

object DataRequest {
  implicit val unsignedDataRequestReads: Format[DataRequest] = Json.format
}

case class DataRequest(
    version: Byte,
    sender: String,
    data: List[DataEntry[?]],
    fee: Long,
    timestamp: Option[Long] = None
)

case class SignedDataRequest(
    version: Byte,
    senderPublicKey: String,
    data: List[DataEntry[?]],
    fee: Long,
    timestamp: Long,
    proofs: Option[Proofs],
    signature: Option[ByteStr]
) extends TxBroadcastRequest[DataTransaction] {
  def toTx: Either[ValidationError, DataTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      validSender <- PublicKey.fromBase58String(senderPublicKey)
      tx          <- DataTransaction.create(version, validSender, data, fee, timestamp, validProofs)
    } yield tx

}

object SignedDataRequest {
  implicit val signedDataRequestReads: Format[SignedDataRequest] = Json.format
}
