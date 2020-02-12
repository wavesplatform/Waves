package com.wavesplatform.api.http

import play.api.libs.json.{Format, Json}

case class ConnectReq(host: String, port: Int)

object ConnectReq {
  implicit val connectFormat: Format[ConnectReq] = Json.format
}
