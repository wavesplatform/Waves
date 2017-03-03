package scorex.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class AliasRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                        sender: String,
                        @ApiModelProperty(required = true)
                        fee: Long,
                        @ApiModelProperty(value = "Alias", required = true)
                        alias: String)

object AliasRequest {
  implicit val aliasRequestFormat: Format[AliasRequest] = Json.format
}
