package com.wavesplatform.api.http

import play.api.libs.json.Json

case class ScriptWithImportsRequest(script: String, imports: Map[String, String] = Map.empty)

object ScriptWithImportsRequest {
  implicit val jsonFormat = Json.format[ScriptWithImportsRequest]
}
