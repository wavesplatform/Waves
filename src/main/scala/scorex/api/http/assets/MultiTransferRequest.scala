package scorex.api.http.assets

import play.api.libs.json.{Format, Json}

case class MultiTransferRequest(assetId: Option[String],
                                sender: String,
                                recipients: Seq[(String, Long)],
                                fee: Long,
                                attachment: Option[String],
                                timestamp: Option[Long] = None)

object MultiTransferRequest {
  implicit val jsonFormat: Format[MultiTransferRequest] = Json.format
}
