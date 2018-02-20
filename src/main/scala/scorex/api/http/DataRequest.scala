package scorex.api.http

import play.api.libs.json.{Format, Json}

case class DataRequest(sender: String,
                       data: Map[String, String],
                       fee: Long,
                       timestamp: Option[Long] = None)

object DataRequest {
  implicit val jsonFormat: Format[DataRequest] = Json.format
}
