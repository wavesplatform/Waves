package com.wavesplatform.api.http.assets

import play.api.libs.json.{Format, JsNumber, JsObject, Json}
import com.wavesplatform.transaction.smart.SetScriptTransaction

case class SetScriptRequest(sender: String, script: Option[String], fee: Long, timestamp: Option[Long] = None) {}

object SetScriptRequest {
  implicit val jsonFormat: Format[SetScriptRequest] = Json.format
  implicit class SetScriptRequestExt(val self: SetScriptRequest) extends AnyVal {
    def toJsObject: JsObject = Json.toJson(self).as[JsObject] + ("type" -> JsNumber(SetScriptTransaction.typeId.toInt))
  }
}
