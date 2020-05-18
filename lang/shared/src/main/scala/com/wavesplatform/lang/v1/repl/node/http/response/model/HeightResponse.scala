package com.wavesplatform.lang.v1.repl.node.http.response.model

import io.circe.{Decoder, DecodingFailure, HCursor}

private[node] case class HeightResponse(height: Long)

object HeightResponse {
  implicit val decoder: Decoder[HeightResponse] = (c: HCursor) =>
      for {
        applicationStatus <- c.downField("applicationStatus").as[Option[String]]
        succeed = applicationStatus.fold(true)(_ == "succeed")
        _ <- Either.cond(succeed, (), DecodingFailure.apply("Failed transaction", List()))
        h <- c.downField("height").as[Int]
      } yield HeightResponse(h)
}
