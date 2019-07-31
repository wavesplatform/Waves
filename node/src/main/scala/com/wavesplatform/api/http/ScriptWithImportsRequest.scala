package com.wavesplatform.api.http

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, Reads, Writes}

import scala.annotation.meta.field

object ScriptWithImportsRequest {
  implicit val reads: Reads[ScriptWithImportsRequest] = Json.reads[ScriptWithImportsRequest]
  implicit val writes: Writes[ScriptWithImportsRequest] = Json.writes[ScriptWithImportsRequest]
}
case class ScriptWithImportsRequest(
    @(ApiModelProperty @field)(required = true)script: String,
    imports: Map[String, String] = Map()
)
