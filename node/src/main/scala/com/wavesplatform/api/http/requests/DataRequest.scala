package com.wavesplatform.api.http.requests

import com.wavesplatform.account.{AddressScheme, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.DataEntry
import com.wavesplatform.transaction.{DataTransaction, Proofs}
import play.api.libs.json.{Format, Json}

case class DataRequest(
    version: Byte,
    senderPublicKey: String,
    data: List[DataEntry[?]],
    fee: Long,
    timestamp: Long,
    proofs: Option[Proofs],
    signature: Option[ByteStr],
    chainId: Byte = AddressScheme.current.chainId
) extends TxBroadcastRequest[DataTransaction] {
  def toTx: Either[ValidationError, DataTransaction] =
    for {
      validProofs <- toProofs(signature, proofs)
      validSender <- PublicKey.fromBase58String(senderPublicKey)
      tx          <- DataTransaction.create(version, validSender, data, fee, timestamp, validProofs, chainId)
    } yield tx

}

object DataRequest {
  given Format[DataRequest] = Json.format
}
