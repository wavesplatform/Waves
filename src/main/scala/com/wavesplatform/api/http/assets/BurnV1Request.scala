package com.wavesplatform.api.http.assets

import play.api.libs.json._

case class BurnV1Request(sender: String, assetId: String, quantity: Long, fee: Long, timestamp: Option[Long] = None)

object BurnV1Request {
  implicit val burnV1Format: Format[BurnV1Request] = Json.format
}
