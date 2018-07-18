package com.wavesplatform.api.http.leasing

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class LeaseCancelV2Request(version: Byte,
                                @ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                sender: String,
                                @ApiModelProperty(value = "Base58 encoded lease transaction id", required = true)
                                txId: String,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                timestamp: Option[Long] = None)

object LeaseCancelV2Request {
  implicit val leaseCancelRequestFormat: Format[LeaseCancelV2Request] = Json.format
}
