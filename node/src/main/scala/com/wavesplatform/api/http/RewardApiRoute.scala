package com.wavesplatform.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.state.Blockchain
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import javax.ws.rs.Path
import play.api.libs.json.Json

@Path("/reward")
@Api(value = "reward")
case class RewardApiRoute(blockchain: Blockchain) extends ApiRoute {
  import RewardApiRoute._

  override lazy val route: Route = pathPrefix("reward") {
    status
  }

  @Path("/status")
  @ApiOperation(value = "Status", notes = "Get current miner’s reward status", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json reward status")
    )
  )
  def status: Route = (get & path("status")) {
    val height = blockchain.height
    complete(Json.toJson(blockchain.activatedFeatures.get(BlockchainFeatures.BlockReward.id).map { activatedAt =>
      val settings = blockchain.settings.functionalitySettings.blockRewardSettings

      val current = blockchain.blockReward

      val blocksBeforeNextVote = {
        import settings._
        if (activatedAt + firstRewardPeriod >= height) activatedAt + firstRewardPeriod - height
        else rewardPeriod - ((height - (activatedAt + firstRewardPeriod)) % rewardPeriod)
      }

      val isVotingPeriod = blocksBeforeNextVote <= settings.rewardVotingPeriod

      RewardStatus(current, blocksBeforeNextVote, isVotingPeriod)
    }))
  }
}

object RewardApiRoute {
  import play.api.libs.json._

  final case class RewardStatus(reward: Long, blocksBeforeNextVote: Int, isVotingPeriod: Boolean)

  implicit val rewardStatusFormat: Format[RewardStatus] = Json.format

  implicit val rewardStatusOptFormat: Format[Option[RewardStatus]] =
    new Format[Option[RewardStatus]] {

      override def reads(json: JsValue): JsResult[Option[RewardStatus]] =
        json.validate[RewardStatus] match {
          case JsError(_) => JsSuccess(None)
          case s          => s.map(Some(_))
        }

      override def writes(o: Option[RewardStatus]): JsValue =
        o match {
          case Some(s) => Json.toJson(s)
          case None    => Json.obj()
        }
    }
}
