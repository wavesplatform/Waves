package com.wavesplatform.lang.v1.repl.node.http.response.model

import io.circe.{Decoder, HCursor}

private[lang] case class HeightResponse(height: Long, succeed: Boolean)

object HeightResponse {
  implicit val decoder: Decoder[HeightResponse] = (c: HCursor) =>
    for {
      applicationStatus <- c.downField("applicationStatus").as[Option[String]]
      succeed = applicationStatus.fold(true)(_ == "succeeded")
      h <- c.downField("height").as[Long]
    } yield HeightResponse(h, succeed)
}
