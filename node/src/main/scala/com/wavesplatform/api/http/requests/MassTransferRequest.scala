package com.wavesplatform.api.http.requests

import com.wavesplatform.transaction.transfer.MassTransferTransaction.Transfer
import play.api.libs.json.{Json, Reads}

case class MassTransferRequest(
    assetId: Option[String],
    sender: String,
    transfers: List[Transfer],
    fee: Long,
    attachment: Option[String],
    timestamp: Option[Long] = None
)

object MassTransferRequest {
  implicit val reads: Reads[MassTransferRequest] = Json.reads
}
