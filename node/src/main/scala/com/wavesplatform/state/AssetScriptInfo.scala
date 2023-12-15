package com.wavesplatform.state

import com.wavesplatform.lang.script.Script
import play.api.libs.json.{JsString, Json, OWrites, Writes}

case class AssetScriptInfo(script: Script, complexity: Long)

object AssetScriptInfo {
  implicit val scriptWrites: Writes[Script]     = Writes[Script](s => JsString(s.bytes().base64))
  implicit val format: OWrites[AssetScriptInfo] = Json.writes[AssetScriptInfo]
}
