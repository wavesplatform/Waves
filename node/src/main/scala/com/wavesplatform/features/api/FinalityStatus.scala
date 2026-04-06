package com.wavesplatform.features.api

import com.wavesplatform.state.{GenerationPeriod, Height}
import play.api.libs.json.*
import play.api.libs.functional.syntax._

case class FinalityStatus(
    height: Height,
    finalizedHeight: Height,
    currentGenerationPeriod: Option[GenerationPeriod],
    nextGenerationPeriod: Option[GenerationPeriod]
)

object FinalityStatus {
  private def generationPeriodReads(activationHeight: Height): Reads[GenerationPeriod] =
    (
      (__ \ "start").read[Height] and (__ \ "end").read[Height]
    )((start, end) => GenerationPeriod(activationHeight, start, end - start))

  given Reads[FinalityStatus] =
    (
      (__ \ "height").read[Height] and
        (__ \ "finalizedHeight").read[Height] and
        (__ \ "activationHeight").read[Height] and
        (__ \ "currentGenerationPeriod").readNullable[JsObject] and
        (__ \ "nextGenerationPeriod").readNullable[JsObject]
    )((h, fh, ah, cgp, ngp) =>
      FinalityStatus(
        h,
        fh,
        cgp.map(_.as[GenerationPeriod](using generationPeriodReads(ah))),
        ngp.map(_.as[GenerationPeriod](using generationPeriodReads(ah)))
      )
    )
}
