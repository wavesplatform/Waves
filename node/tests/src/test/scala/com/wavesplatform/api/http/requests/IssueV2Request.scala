package com.wavesplatform.api.http.requests

import com.wavesplatform.transaction.assets.IssueTransaction
import play.api.libs.json.{Format, JsNumber, JsObject, Json}

case class IssueV2Request(
    sender: String,
    name: String,
    description: String,
    quantity: Long,
    decimals: Byte,
    reissuable: Boolean,
    script: Option[String],
    fee: Long,
    timestamp: Option[Long]
)

object IssueV2Request {
  implicit val jsonFormat: Format[IssueV2Request] = Json.format
  implicit class SmartIssueRequestExt(val self: IssueV2Request) extends AnyVal {
    def toJsObject: JsObject = Json.toJson(self).as[JsObject] + ("type" -> JsNumber(IssueTransaction.typeId.toInt))
  }
}
