package com.wavesplatform.features.api

import play.api.libs.json.{Json, Writes}

case class ActivationStatus(height: Int,
                            votingInterval: Int,
                            votingThreshold: Int,
                            nextCheck: Int,
                            features: Set[ActivationStatusFeature])

object ActivationStatus {
  implicit val activationStatusWrites = new Writes[ActivationStatus] {
    def writes(as: ActivationStatus) = Json.obj(
      "height" -> as.height,
      "votingInterval" -> as.votingInterval,
      "votingThreshold" -> as.votingThreshold,
      "nextCheck" -> as.nextCheck,
      "features" -> Json.arr(as.features)
    )
  }
}