package scorex.api.http.alias

import play.api.libs.json.{Format, Json}


case class CreateAliasRequest(sender: String, alias: String, fee: Long)

object CreateAliasRequest {
  implicit val burnFormat: Format[CreateAliasRequest] = Json.format
}

