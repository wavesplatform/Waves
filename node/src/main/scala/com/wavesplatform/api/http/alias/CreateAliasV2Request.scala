package com.wavesplatform.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class CreateAliasV2Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                sender: String,
                                @ApiModelProperty(value = "Alias", required = true)
                                alias: String,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                timestamp: Option[Long] = None)

object CreateAliasV2Request {
  implicit val aliasV2RequestFormat: Format[CreateAliasV2Request] = Json.format
}
