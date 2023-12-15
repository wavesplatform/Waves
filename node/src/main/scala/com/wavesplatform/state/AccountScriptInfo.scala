package com.wavesplatform.state

import com.wavesplatform.account.PublicKey
import com.wavesplatform.lang.script.Script
import play.api.libs.json.{JsString, Json, OWrites, Writes}

case class AccountScriptInfo(
    publicKey: PublicKey,
    script: Script,
    verifierComplexity: Long,
    complexitiesByEstimator: Map[Int, Map[String, Long]] = Map.empty
)

object AccountScriptInfo {
  implicit val scriptWrites: Writes[Script]       = Writes[Script](s => JsString(s.bytes().base64))
  implicit val writes: OWrites[AccountScriptInfo] = Json.writes
}
