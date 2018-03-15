package scorex.api.http.assets

import play.api.libs.json.{Format, Json}

case class VersionedTransferRequest(version: Byte,
                                    assetId: Option[String],
                                    amount: Long,
                                    fee: Long,
                                    sender: String,
                                    attachment: Option[String],
                                    recipient: String,
                                    timestamp: Option[Long] = None)

object VersionedTransferRequest {
  implicit val versionedTransferFormat: Format[VersionedTransferRequest] = Json.format
}
