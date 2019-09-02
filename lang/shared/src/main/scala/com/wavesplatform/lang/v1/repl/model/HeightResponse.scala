package com.wavesplatform.lang.v1.repl.model

import upickle.default._

case class HeightResponse(height: Long)
object HeightResponse {
  implicit val rw: ReadWriter[HeightResponse] = macroRW
}