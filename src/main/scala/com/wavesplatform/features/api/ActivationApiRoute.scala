package com.wavesplatform.features.api

import javax.ws.rs.Path

import akka.http.scaladsl.server.Route
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.{FeaturesSettings, FunctionalitySettings, RestAPISettings}
import io.swagger.annotations._
import play.api.libs.json._
import scorex.api.http.{ApiRoute, CommonApiFunctions}
import scorex.transaction.History
import scorex.utils.ScorexLogging

@Path("/activation")
@Api(value = "activation")
case class ActivationApiRoute(settings: RestAPISettings,
                        functionalitySettings: FunctionalitySettings,
                        featuresSettings: FeaturesSettings,
                        history: History,
                        featureProvider: FeatureProvider)
  extends ApiRoute with CommonApiFunctions with ScorexLogging {

  override lazy val route = pathPrefix("activation") {
    status
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get activation status", httpMethod = "GET")
  @ApiResponses(Array(
    new ApiResponse(code = 200, message = "Json activation status")
  ))
  def status: Route = (get & path("status")) {

    val height = history.height()
    val activationInterval = functionalitySettings.featureCheckBlocksPeriod

    complete(Json.toJson(
      ActivationStatus(height,
        activationInterval,
        functionalitySettings.blocksForFeatureActivation,
        (FeatureProvider.votingWindowOpeningFromHeight(height, activationInterval) + activationInterval) - 1,
        (featureProvider.featureVotesCountWithinActivationWindow(height).keySet ++
          featureProvider.approvedFeatures().keySet ++
          BlockchainFeatures.implemented).toSeq.sorted.map(id => {
          val status = featureProvider.featureStatus(id, height)
          ActivationStatusFeature(id,
            status,
            (BlockchainFeatures.implemented.contains(id), featuresSettings.supported.contains(id)) match {
              case (false, _) => NodeFeatureStatus.NotImplemented
              case (_, true) => NodeFeatureStatus.Voted
              case _ => NodeFeatureStatus.Implemented
            },
            featureProvider.featureActivationHeight(id),
            if(status == BlockchainFeatureStatus.Undefined) featureProvider.featureVotesCountWithinActivationWindow(height).get(id).orElse(Some(0)) else None
          )
        })))
    )
  }
}
