package com.wavesplatform.features.api

import com.wavesplatform.features.BlockchainFeatureStatus
import play.api.libs.json.{JsNumber, Json, Writes}



case class ActivationStatusFeature(id: Short,
                                   blockchainStatus: BlockchainFeatureStatus,
                                   nodeStatus: NodeFeatureStatus,
                                   activationHeight: Option[Int],
                                   supportedBlocks: Option[Int])

object ActivationStatusFeature {
  implicit val activationStatuFeatureWrites = new Writes[ActivationStatusFeature] {
    def writes(asf: ActivationStatusFeature) = {
      Json.obj(
        "id" -> asf.id,
        "blockhainStatus" -> asf.blockchainStatus.toString,
        "nodeStatus" -> asf.nodeStatus.toString,
        "activationHeight" -> Json.toJson(asf.activationHeight.getOrElse(0)),
        "supportedBlocks" -> Json.toJson(asf.supportedBlocks.getOrElse(0)),
      )
    }
  }
}
