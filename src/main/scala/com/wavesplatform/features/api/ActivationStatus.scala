package com.wavesplatform.features.api

import play.api.libs.json.{Json, Writes}

case class ActivationStatus(height: Int,
                            approvalInterval: Int,
                            approvalThreshold: Int,
                            nextCheck: Int,
                            features: Set[ActivationStatusFeature])

object ActivationStatus {
  implicit val activationStatusWrites = new Writes[ActivationStatus] {
    def writes(as: ActivationStatus) = Json.obj(
      "height" -> as.height,
      "approvalInterval" -> as.approvalInterval,
      "approvalTreshold" -> as.approvalThreshold,
      "nextCheck" -> as.nextCheck,
    )
  }
}