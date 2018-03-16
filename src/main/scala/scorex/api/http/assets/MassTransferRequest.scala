package scorex.api.http.assets

import play.api.libs.json.{Format, Json}
import scorex.transaction.assets.MassTransferTransaction.Transfer

case class MassTransferRequest(version: Byte,
                               assetId: Option[String],
                               sender: String,
                               transfers: List[Transfer],
                               fee: Long,
                               attachment: Option[String],
                               timestamp: Option[Long] = None)

object MassTransferRequest {
  implicit val jsonFormat: Format[MassTransferRequest] = Json.format
}
