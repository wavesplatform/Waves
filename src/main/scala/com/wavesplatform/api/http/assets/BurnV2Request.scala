package com.wavesplatform.api.http.assets

import play.api.libs.json.{Format, Json}

case class BurnV2Request(version: Byte, sender: String, assetId: String, quantity: Long, fee: Long, timestamp: Option[Long] = None)

object BurnV2Request {
  implicit val burnV2Format: Format[BurnV2Request] = Json.format
}
