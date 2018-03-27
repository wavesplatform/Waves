package scorex.api.http.alias

import io.swagger.annotations.ApiModelProperty
import play.api.libs.json.{Format, Json}

case class CreateAliasRequest(@ApiModelProperty(value = "Base58 encoded sender public key", required = true)
                              sender: String,
                              @ApiModelProperty(value = "Alias", required = true)
                              alias: String,
                              @ApiModelProperty(required = true)
                              fee: Long,
                              timestamp: Option[Long] = None)

object CreateAliasRequest {
  implicit val aliasRequestFormat: Format[CreateAliasRequest] = Json.format
}
