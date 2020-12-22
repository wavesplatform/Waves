package com.wavesplatform.api.http

import play.api.libs.json.{Format, Json}

case class DebugMessage(message: String)

object DebugMessage {
  implicit val debugMessageFormat: Format[DebugMessage] = Json.format
}
