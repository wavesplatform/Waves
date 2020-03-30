package com.wavesplatform.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.features.FeatureProvider._
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxValidationError.GenericError
import play.api.libs.json.{Format, Json}

case class RewardApiRoute(blockchain: Blockchain) extends ApiRoute {
  import RewardApiRoute._

  override lazy val route: Route = pathPrefix("blockchain" / "rewards") {
    rewards()
  }

  def rewards(): Route = (get & pathEndOrSingleSlash) {
    complete(loadRewards())
  }

  def loadRewards(): Either[ValidationError, RewardStatus] =
    for {
      activatedAt <- blockchain
        .featureActivationHeight(BlockchainFeatures.BlockReward.id)
        .filter(_ <= blockchain.height)
        .toRight(GenericError("Block reward feature is not activated yet"))
      reward <- blockchain.blockReward(blockchain.height).toRight(GenericError(s"No information about rewards at height = ${blockchain.height}"))
      amount              = blockchain.wavesAmount(blockchain.height)
      settings            = blockchain.settings.rewardsSettings
      nextCheck           = settings.nearestTermEnd(activatedAt, blockchain.height)
      votingIntervalStart = nextCheck - settings.votingInterval + 1
      votingThreshold     = settings.votingInterval / 2 + 1
      votes               = blockchain.blockRewardVotes.filter(_ >= 0)
    } yield RewardStatus(
      blockchain.height,
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
