package com.wavesplatform.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxValidationError.GenericError
import io.swagger.annotations._
import javax.ws.rs.Path
import play.api.libs.json.{Format, Json}

@Path("/blockchain/rewards")
@Api(value = "rewards")
case class RewardApiRoute(blockchain: Blockchain) extends ApiRoute {
  import RewardApiRoute._

  override lazy val route: Route = pathPrefix("blockchain" / "rewards") {
    rewards ~ rewardsAtHeight()
  }

  @Path("/")
  @ApiOperation(value = "Current reward status", notes = "Get current miner’s reward status", httpMethod = "GET")
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json reward status")
    )
  )
  def rewards(): Route = (get & pathEndOrSingleSlash) {
    complete(getRewards(blockchain.height))
  }

  @Path("/{height}")
  @ApiOperation(value = "Reward status", notes = "Get miner’s reward status at height", httpMethod = "GET")
  @ApiImplicitParams(
    Array(
      new ApiImplicitParam(name = "height", value = "Target block height", required = true, dataType = "integer", paramType = "path")
    )
  )
  @ApiResponses(
    Array(
      new ApiResponse(code = 200, message = "Json reward status")
    )
  )
  def rewardsAtHeight(): Route = (get & path(IntNumber)) { height =>
    complete(getRewards(height))
  }

  def getRewards(height: Int): Either[ValidationError, RewardStatus] =
    for {
      _ <- Either.cond(height <= blockchain.height, (), GenericError(s"Invalid height: $height"))
      activatedAt <- blockchain
        .featureActivationHeight(BlockchainFeatures.BlockReward.id)
        .filter(_ <= height)
        .toRight(GenericError("Block reward feature is not activated yet"))
      reward <- blockchain.blockReward(height).toRight(GenericError(s"No information about rewards at height = $height"))
      amount              = blockchain.wavesAmount(height)
      settings            = blockchain.settings.rewardsSettings
      nextCheck           = settings.nearestTermEnd(activatedAt, height)
      votingIntervalStart = nextCheck - settings.votingInterval + 1
      votingThreshold     = settings.votingInterval / 2 + 1
      votes               = blockchain.blockRewardVotes(height).filter(_ >= 0)
    } yield RewardStatus(
      height,
      amount,
      reward,
      settings.minIncrement,
      settings.term,
      nextCheck,
      votingIntervalStart,
      settings.votingInterval,
      votingThreshold,
      RewardVotes(votes.count(_ > reward), votes.count(_ < reward))
    )
}

object RewardApiRoute {
  final case class RewardStatus(
      height: Int,
      totalWavesAmount: BigInt,
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
  implicit val rewardFormat: Format[RewardStatus]     = Json.format
}
