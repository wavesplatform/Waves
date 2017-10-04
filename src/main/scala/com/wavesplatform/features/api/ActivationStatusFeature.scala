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
      val json = Json.obj(
        "id" -> asf.id,
        "blockhainStatus" -> asf.blockchainStatus.toString,
        "nodeStatus" -> asf.nodeStatus.toString
      )
       //asf.activationHeight.map("name" -> JsNumber(_)).getOrElse(())

      Seq(asf.activationHeight, asf.supportedBlocks).flatten.foldLeft(json)((j, o) => j + )
    }
  }
}
