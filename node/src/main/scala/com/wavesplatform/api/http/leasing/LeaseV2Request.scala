package com.wavesplatform.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class LeaseV2Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                          sender: String,
                          @ApiModelProperty(required = true)
                          amount: Long,
                          @ApiModelProperty(required = true)
                          fee: Long,
                          @ApiModelProperty(value = "Recipient address", required = true)
                          recipient: String,
                          timestamp: Option[Long] = None)

object LeaseV2Request {
  implicit val leaseCancelRequestFormat: Format[LeaseV2Request] = Json.format
}
