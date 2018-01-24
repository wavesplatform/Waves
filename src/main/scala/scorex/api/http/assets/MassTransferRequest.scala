package scorex.api.http.assets

import play.api.libs.json.{Format, Json}

case class MassTransferRequest(assetId: Option[String],
                               sender: String,
                               transfers: List[(String, Long)],
                               fee: Long,
                               attachment: Option[String],
                               timestamp: Option[Long] = None)

object MassTransferRequest {
  implicit val jsonFormat: Format[MassTransferRequest] = Json.format
}
