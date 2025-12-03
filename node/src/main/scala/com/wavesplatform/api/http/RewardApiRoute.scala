package com.wavesplatform.api.http

import com.wavesplatform.features.BlockchainFeatures
import com.wavesplatform.lang.ValidationError
import com.wavesplatform.state.{Blockchain, Height}
import com.wavesplatform.transaction.TxValidationError.GenericError
import org.apache.pekko.http.scaladsl.server.Route
import play.api.libs.json.JsonConfiguration.Aux
import play.api.libs.json.{Json, JsonConfiguration, OptionHandlers, Writes}

case class RewardApiRoute(blockchain: Blockchain) extends ApiRoute {
  import RewardApiRoute.*

  override lazy val route: Route = pathPrefix("blockchain" / "rewards") {
    rewards() ~ rewardsAtHeight()
  }

  def rewards(): Route = (get & pathEndOrSingleSlash) {
    complete(getRewards(Height(blockchain.height)))
  }

  def rewardsAtHeight(): Route = (get & path(IntNumber)) { height =>
    complete(getRewards(Height(height)))
  }

  def getRewards(height: Height): Either[ValidationError, RewardStatus] =
    for {
      _ <- Either.cond(height.toInt <= blockchain.height, (), GenericError(s"Invalid height: $height"))
      activatedAt <- blockchain
        .featureActivationHeight(BlockchainFeatures.BlockReward)
        .filter(_ <= height)
        .toRight(GenericError("Block reward feature is not activated yet"))
      reward <- blockchain.blockReward(height.toInt).toRight(GenericError(s"No information about rewards at height = $height"))
      amount          = blockchain.wavesAmount(height.toInt)
      rewardsSettings = blockchain.settings.rewardsSettings
      funcSettings    = blockchain.settings.functionalitySettings
      nextCheck = rewardsSettings.nearestTermEnd(activatedAt, height, blockchain.isFeatureActivated(BlockchainFeatures.CappedReward, height.toInt))
      votingIntervalStart = nextCheck - rewardsSettings.votingInterval + 1
      votingThreshold     = rewardsSettings.votingInterval / 2 + 1
      votes               = blockchain.blockRewardVotes(height.toInt).filter(_ >= 0)
      term =
        if (blockchain.isFeatureActivated(BlockchainFeatures.CappedReward, height.toInt))
          rewardsSettings.termAfterCappedRewardFeature
        else rewardsSettings.term
    } yield RewardStatus(
      height,
      amount,
      reward * blockchain.blockRewardBoost(Height(height.toInt)),
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
      height: Height,
      totalWavesAmount: BigInt,
      currentReward: Long,
      minIncrement: Long,
      term: Int,
      nextCheck: Height,
      votingIntervalStart: Height,
      votingInterval: Int,
      votingThreshold: Int,
      votes: RewardVotes,
      daoAddress: Option[String],
      xtnBuybackAddress: Option[String]
  )

  final case class RewardVotes(increase: Int, decrease: Int)

  given Aux[Json.MacroOptions] = JsonConfiguration(optionHandlers = OptionHandlers.WritesNull)

  given Writes[RewardVotes]  = Json.writes
  given Writes[RewardStatus] = Json.writes
}
