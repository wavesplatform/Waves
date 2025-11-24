package com.wavesplatform.features.api

import com.wavesplatform.api.http.ApiRoute
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.settings.{FeaturesSettings, RestAPISettings}
import com.wavesplatform.state.{Blockchain, Height}
import org.apache.pekko.http.scaladsl.server.Route
import play.api.libs.json.Json

case class ActivationApiRoute(settings: RestAPISettings, featuresSettings: FeaturesSettings, blockchain: Blockchain) extends ApiRoute {

  override lazy val route: Route = pathPrefix("activation") {
    status
  }

  def status: Route = (get & path("status")) {
    val height = Height(blockchain.height)

    val featureIds = (blockchain.featureVotes(height).keySet ++
      blockchain.approvedFeatures.keySet ++
      BlockchainFeatures.implemented).toSeq.sorted

    complete(
      Json.toJson(
        ActivationStatus(
          height,
          blockchain.settings.functionalitySettings.activationWindowSize(height.toInt),
          blockchain.settings.functionalitySettings.blocksForFeatureActivation(height.toInt),
          Height(blockchain.settings.functionalitySettings.activationWindow(height.toInt).last),
          featureIds.map { id =>
            val status = blockchain.featureStatus(id, height.toInt)
            val voted = featuresSettings.supported.contains(id) && !blockchain.activatedFeatures
              .get(id)
              .exists(_ <= height) && !blockchain.settings.functionalitySettings.preActivatedFeatures.contains(id)

            FeatureActivationStatus(
              id,
              BlockchainFeatures.feature(id).fold("Unknown feature")(_.description),
              status,
              (BlockchainFeatures.implemented.contains(id), voted) match {
                case (false, _) => NodeFeatureStatus.NotImplemented
                case (_, true)  => NodeFeatureStatus.Voted
                case _          => NodeFeatureStatus.Implemented
              },
              blockchain.featureActivationHeight(id),
              if (status == BlockchainFeatureStatus.Undefined) blockchain.featureVotes(height).get(id).orElse(Some(0)) else None
            )
          }
        )
      )
    )
  }
}
