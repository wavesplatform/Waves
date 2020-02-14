package com.wavesplatform.features.api

import akka.http.scaladsl.server.Route
import com.wavesplatform.api.http.ApiRoute
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.settings.{FeaturesSettings, RestAPISettings}
import com.wavesplatform.state.Blockchain
import play.api.libs.json.Json

case class ActivationApiRoute(settings: RestAPISettings, featuresSettings: FeaturesSettings, blockchain: Blockchain) extends ApiRoute {

  override lazy val route: Route = pathPrefix("activation") {
    status
  }

  def status: Route = (get & path("status")) {
    val height = blockchain.height

    complete(
      Json.toJson(
        ActivationStatus(
          height,
          blockchain.settings.functionalitySettings.activationWindowSize(height),
          blockchain.settings.functionalitySettings.blocksForFeatureActivation(height),
          blockchain.settings.functionalitySettings.activationWindow(height).last,
          (blockchain.featureVotes(height).keySet ++
            blockchain.approvedFeatures.keySet ++
            BlockchainFeatures.implemented).toSeq.sorted.map(id => {
            val status = blockchain.featureStatus(id, height)
            FeatureActivationStatus(
              id,
              BlockchainFeatures.feature(id).fold("Unknown feature")(_.description),
              status,
              (BlockchainFeatures.implemented.contains(id), featuresSettings.supported.contains(id)) match {
                case (false, _) => NodeFeatureStatus.NotImplemented
                case (_, true)  => NodeFeatureStatus.Voted
                case _          => NodeFeatureStatus.Implemented
              },
              blockchain.featureActivationHeight(id),
              if (status == BlockchainFeatureStatus.Undefined) blockchain.featureVotes(height).get(id).orElse(Some(0)) else None
            )
          })
        )
      )
    )
  }
}
