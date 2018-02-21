package scorex.api.http

import play.api.libs.json.{Format, Json}
import scorex.transaction.DataTransaction.Data

case class DataRequest(sender: String,
                       data: Data,
                       fee: Long,
                       timestamp: Option[Long] = None)

object DataRequest {
  implicit val jsonFormat: Format[DataRequest] = Json.format
}
