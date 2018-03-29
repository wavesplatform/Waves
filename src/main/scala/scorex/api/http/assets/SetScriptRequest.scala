package scorex.api.http.assets

import play.api.libs.json.{Format, Json}

case class SetScriptRequest(version: Byte, sender: String, script: Option[String], fee: Long, timestamp: Option[Long] = None)

object SetScriptRequest {
  implicit val jsonFormat: Format[SetScriptRequest] = Json.format
}
