package com.wavesplatform.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Json, Reads, Writes}

case class LeaseCancelV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                sender: String,
                                @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                txId: String,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                timestamp: Option[Long] = None)

object LeaseCancelV1Request {
  implicit val leaseCancelRequestReads: Reads[LeaseCancelV1Request] = {
    import play.api.libs.json._
    import play.api.libs.functional.syntax._

    ((JsPath \ 'sender).read[String] ~
      ((JsPath \ 'txId).read[String] | (JsPath \ 'leaseId).read[String]) ~
      (JsPath \ 'fee).read[Long] ~
      (JsPath \ 'timestamp).readNullable[Long])(LeaseCancelV1Request.apply _)
  }

  implicit val leaseCancelRequestWrites: Writes[LeaseCancelV1Request] = Json.writes[LeaseCancelV1Request]
}
