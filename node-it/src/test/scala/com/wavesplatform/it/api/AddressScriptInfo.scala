package com.wavesplatform.it.api

import play.api.libs.json.{Format, Json}

case class AddressScriptInfo(address: String, script: Option[String], scriptText: Option[String], complexity: Long, extraFee: Long)

object AddressScriptInfo {
  implicit val accountScriptInfoFormat: Format[AddressScriptInfo] = Json.format
}
