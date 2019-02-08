package com.wavesplatform.features.api

import akka.http.scaladsl.server.Route
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.features.{BlockchainFeatureStatus, BlockchainFeatures}
import com.wavesplatform.settings.{FeaturesSettings, FunctionalitySettings, RestAPISettings}
import com.wavesplatform.state.Blockchain
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.Json
import com.wavesplatform.api.http.{ApiRoute, CommonApiFunctions}
import com.wavesplatform.utils.ScorexLogging

@Path("/activation")
@Api(value = "activation")
case class ActivationApiRoute(settings: RestAPISettings,
                              functionalitySettings: FunctionalitySettings,
                              featuresSettings: FeaturesSettings,
                              blockchain: Blockchain)
    extends ApiRoute
    with CommonApiFunctions
    with ScorexLogging {

  override lazy val route: Route = pathPrefix("activation") {
    status
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get activation status", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json activation status")
    ))
  def status: Route = (get & path("status")) {

    val height = blockchain.height

    complete(
      Json.toJson(ActivationStatus(
        height,
        functionalitySettings.activationWindowSize(height),
        functionalitySettings.blocksForFeatureActivation(height),
        functionalitySettings.activationWindow(height).last,
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
      )))
  }
}
