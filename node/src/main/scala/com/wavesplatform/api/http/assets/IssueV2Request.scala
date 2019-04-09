package com.wavesplatform.api.http.assets

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, JsNumber, JsObject, Json}
import com.wavesplatform.transaction.assets.IssueTransactionV2

case class IssueV2Request(@ApiModelProperty(value = "Base58 encoded Issuer public key", required = true)
                          sender: String,
                          @ApiModelProperty(value = "Name of Asset", required = true)
                          name: String,
                          @ApiModelProperty(value = "Description of Asset", required = true)
                          description: String,
                          @ApiModelProperty(required = true, example = "1000000")
                          quantity: Long,
                          @ApiModelProperty(allowableValues = "range[0,8]", example = "8", dataType = "integer", required = true)
                          decimals: Byte,
                          @ApiModelProperty(required = true)
                          reissuable: Boolean,
                          @ApiModelProperty(value = "Base58 encoded compiled asset script")
                          script: Option[String],
                          @ApiModelProperty(required = true)
                          fee: Long,
                          timestamp: Option[Long]) {}

object IssueV2Request {
  implicit val jsonFormat: Format[IssueV2Request] = Json.format
  implicit class SmartIssueRequestExt(val self: IssueV2Request) extends AnyVal {
    def toJsObject: JsObject = Json.toJson(self).as[JsObject] + ("type" -> JsNumber(IssueTransactionV2.typeId.toInt))
  }
}
