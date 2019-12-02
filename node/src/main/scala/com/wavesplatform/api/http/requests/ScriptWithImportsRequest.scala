package com.wavesplatform.api.http.requests

import play.api.libs.json.{Json, Reads, Writes}

object ScriptWithImportsRequest {
  implicit val reads: Reads[ScriptWithImportsRequest]   = Json.reads[ScriptWithImportsRequest]
  implicit val writes: Writes[ScriptWithImportsRequest] = Json.writes[ScriptWithImportsRequest]
}
case class ScriptWithImportsRequest(
    script: String,
    imports: Map[String, String] = Map()
)
