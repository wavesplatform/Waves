package scorex.api.http.assets

import play.api.libs.json.{Format, Json}

case class ReissueRequest(sender: String, assetId: String, quantity: Long, reissuable: Boolean, fee: Long, timestamp: Option[Long] = None)

object ReissueRequest {
  implicit val reissueFormat: Format[ReissueRequest] = Json.format
}
