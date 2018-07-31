package com.wavesplatform.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class CreateAliasV1Request(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                                sender: String,
                                @ApiModelProperty(value = "Alias", required = true)
                                alias: String,
                                @ApiModelProperty(required = true)
                                fee: Long,
                                timestamp: Option[Long] = None)

object CreateAliasV1Request {
  implicit val aliasV1RequestFormat: Format[CreateAliasV1Request] = Json.format
}
