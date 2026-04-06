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
  def parse(activationHeight: Option[Height]): Reads[FinalityStatus] =
    Reads { json =>
      for {
        height                  <- (json \ "height").validate[Height]
        finalizedHeight         <- (json \ "finalizedHeight").validate[Height]
        currentGenerationPeriod <- readGenerationPeriod(activationHeight, json, "currentGenerationPeriod")
        nextGenerationPeriod    <- readGenerationPeriod(activationHeight, json, "nextGenerationPeriod")
      } yield FinalityStatus(height, finalizedHeight, currentGenerationPeriod, nextGenerationPeriod)

    }

  private def readGenerationPeriod(activationHeight: Option[Height], json: JsValue, fieldName: String) =
    activationHeight.fold(JsError())(h => (json \ fieldName).validateOpt[GenerationPeriod](using generationPeriodReads(h)))

  private def generationPeriodReads(activationHeight: Height): Reads[GenerationPeriod] =
    (
      (__ \ "start").read[Height] and (__ \ "end").read[Height]
    )((start, end) => GenerationPeriod(activationHeight, start, end - start))
}
