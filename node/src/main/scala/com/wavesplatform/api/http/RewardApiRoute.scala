package com.wavesplatform.api.http

import akka.http.scaladsl.server.Route
import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.Blockchain
import com.wavesplatform.transaction.TxValidationError.GenericError
import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json.{Format, Json, JsonConfiguration, OptionHandlers}

case class RewardApiRoute(blockchain: Blockchain) extends ApiRoute {
  import RewardApiRoute.*

  override lazy val route: Route = pathPrefix("blockchain" / "rewards") {
    rewards() ~ rewardsAtHeight()
  }

  def rewards(): Route = (get & pathEndOrSingleSlash) {
    complete(getRewards(blockchain.height))
  }

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
      amount          = blockchain.wavesAmount(height)
      rewardsSettings = blockchain.settings.rewardsSettings
      funcSettings    = blockchain.settings.functionalitySettings
      nextCheck       = rewardsSettings.nearestTermEnd(activatedAt, height, blockchain.isFeatureActivated(BlockchainFeatures.CappedReward, height))
      votingIntervalStart = nextCheck - rewardsSettings.votingInterval + 1
      votingThreshold     = rewardsSettings.votingInterval / 2 + 1
      votes               = blockchain.blockRewardVotes(height).filter(_ >= 0)
      term =
        if (blockchain.isFeatureActivated(BlockchainFeatures.CappedReward, height))
          rewardsSettings.termAfterCappedRewardFeature
        else rewardsSettings.term
    } yield RewardStatus(
      height,
      amount,
      reward * blockchain.blockRewardBoost(height),
      rewardsSettings.minIncrement,
      term,
      nextCheck,
      votingIntervalStart,
      rewardsSettings.votingInterval,
      votingThreshold,
      RewardVotes(votes.count(_ > reward), votes.count(_ < reward)),
      funcSettings.daoAddress,
      funcSettings.xtnBuybackAddress
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
      votes: RewardVotes,
      daoAddress: Option[String],
      xtnBuybackAddress: Option[String]
  )

  final case class RewardVotes(increase: Int, decrease: Int)

  implicit val config: Aux[Json.MacroOptions] = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)

  implicit val rewardVotesFormat: Format[RewardVotes] = Json.format
  implicit val rewardFormat: Format[RewardStatus]     = Json.format
}
