package com.wavesplatform.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.state.Blockchain
import io.swagger.annotations.{Api, ApiOperation, ApiResponse, ApiResponses}
import javax.ws.rs.Path
import play.api.libs.json.{JsValue, Json}

@Path("/reward")
@Api(value = "reward")
case class RewardApiRoute(blockchain: Blockchain) extends ApiRoute {
  import RewardApiRoute._

  override lazy val route: Route = pathPrefix("reward") {
    status ~ statusAtHeight ~ wavesAmount ~ wavesAmountAtHeight
  }

  @Path("/status")
  @ApiOperation(value = "Current status", notes = "Get current miner’s reward status", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json reward status")
    )
  )
  def status: Route = (get & path("status")) {
    complete(getStatus(blockchain.height))
  }

  @Path("/status/{{height}}")
  @ApiOperation(value = "Status", notes = "Get miner’s reward status at height", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json reward status")
    )
  )
  def statusAtHeight: Route = (get & path("status" / IntNumber)) { height =>
    complete(getStatus(height))
  }

  @Path("/wavesAmount")
  @ApiOperation(value = "Current waves amount", notes = "Get current waves amount", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json reward status")
    )
  )
  def wavesAmount: Route = (get & path("wavesAmount")) {
    complete(getWavesAmount(blockchain.height))
  }

  @Path("/wavesAmount/{{height}}")
  @ApiOperation(value = "Waves amount", notes = "Get waves amount at height", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json reward status")
    )
  )
  def wavesAmountAtHeight: Route = (get & path("wavesAmount" / IntNumber)) { height =>
    complete(getWavesAmount(height))
  }

  private def getStatus(height: Int): JsValue =
    Json.toJson {
      val settings = blockchain.settings.rewardsSettings
      for {
        activatedAt <- blockchain.featureActivationHeight(BlockchainFeatures.BlockReward.id)
        reward      <- blockchain.blockReward(height)
        start = {
          val diff = height - activatedAt
          activatedAt + diff / settings.term * settings.term + 1
        }
        end         = start + settings.term - 1
        votingStart = end - settings.votingInterval + 1
        isVoting    = Range.inclusive(end - settings.votingInterval, end).contains(height)
      } yield RewardStatus(reward, start, votingStart, end, isVoting)
    }

  private def getWavesAmount(height: Int): JsValue =
    Json.toJson {
      if (height < blockchain.height)
        Json.obj("status" -> "error", "details" -> "No amount for this height")
      else
        Json.obj("amount" -> blockchain.wavesAmount(height))
    }
}

object RewardApiRoute {

  import play.api.libs.json._

  final case class RewardStatus(reward: Long, periodStart: Int, votingPeriodStart: Int, periodEnd: Int, isVotingPeriod: Boolean)

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

  final case class Reward(
      height: Int,
      totalWavesAmount: Long,
      currentReward: Long,
      minIncrement: Long,
      term: Int,
      nextCheck: Int,
      votingIntervalStart: Int,
      votingInterval: Int,
      votingThreshold: Int,
      votes: RewardVotes
  )

  final case class RewardVotes(increase: Int, decrease: Int)

  implicit val rewardVotesFormat: Format[RewardVotes] = Json.format
  implicit val rewardFormat: Format[Reward]           = Json.format

}
