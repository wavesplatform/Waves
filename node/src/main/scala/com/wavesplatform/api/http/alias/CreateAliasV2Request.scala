package com.wavesplatform.api.http.alias

import play.api.libs.json.{Format, Json}

case class CreateAliasV2Request(sender: String,
                                alias: String,
                                fee: Long,
                                timestamp: Option[Long] = None)

object CreateAliasV2Request {
  implicit val aliasV2RequestFormat: Format[CreateAliasV2Request] = Json.format
}
