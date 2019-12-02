package com.wavesplatform.api.http.requests

import play.api.libs.json.{Format, Json}

case class CreateAliasV1Request(sender: String, alias: String, fee: Long, timestamp: Option[Long] = None)

object CreateAliasV1Request {
  implicit val aliasV1RequestFormat: Format[CreateAliasV1Request] = Json.format
}
