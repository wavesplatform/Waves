package com.wavesplatform.api.http.assets

import play.api.libs.json.{Format, JsNumber, JsObject, Json}
import com.wavesplatform.transaction.assets.SetAssetScriptTransaction

case class SetAssetScriptRequest(sender: String, assetId: String, script: Option[String], fee: Long, timestamp: Option[Long] = None) {}

object SetAssetScriptRequest {
  implicit val jsonFormat: Format[SetAssetScriptRequest] = Json.format
  implicit class SetAssetScriptRequestExt(val self: SetAssetScriptRequest) extends AnyVal {
    def toJsObject: JsObject = Json.toJson(self).as[JsObject] + ("type" -> JsNumber(SetAssetScriptTransaction.typeId.toInt))
  }
}
