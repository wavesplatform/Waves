package com.wavesplatform.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json._

case class LeaseCancelV2Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                sender: String,
                                @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                txId: String,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                timestamp: Option[Long] = None)

object LeaseCancelV2Request {
  implicit val leaseCancelRequestReads: Reads[LeaseCancelV2Request] = {
    import play.api.libs.json._
    import play.api.libs.functional.syntax._

    ((JsPath \ 'sender).read[String] ~
      ((JsPath \ 'txId).read[String] | (JsPath \ 'leaseId).read[String]) ~
      (JsPath \ 'fee).read[Long] ~
      (JsPath \ 'timestamp).readNullable[Long])(LeaseCancelV2Request.apply _)
  }

  implicit val leaseCancelRequestWrites: Writes[LeaseCancelV2Request] =
    Json.writes[LeaseCancelV2Request].transform((request: JsObject) => request + ("version" -> JsNumber(2)))
}
