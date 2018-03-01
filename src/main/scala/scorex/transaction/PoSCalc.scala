package scorex.transaction

import com.wavesplatform.crypto
import com.wavesplatform.features.{BlockchainFeatures, FeatureProvider}
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.SnapshotStateReader
import scorex.account.{Address, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

object PoSCalc extends ScorexLogging {

  val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L

  private val AvgBlockTimeDepth: Int = 3

  // Min BaseTarget value is 9 because only in this case it is possible to get to next integer value (10)
  // then increasing base target by 11% and casting it to Long afterward (see lines 55 and 59)
  private val MinBaseTarget: Long = 9

  private val MinBlockDelaySeconds = 53
  private val MaxBlockDelaySeconds = 67
  private val BaseTargetGamma = 64

  def calcTarget(prevBlockTimestamp: Long, prevBlockBaseTarget: Long, timestamp: Long, balance: Long): BigInt = {
    val eta = (timestamp - prevBlockTimestamp) / 1000
    BigInt(prevBlockBaseTarget) * eta * balance
  }

  def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8).reverse)

  def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): Array[Byte] =
    crypto.fastHash(lastBlockData.generationSignature.arr ++ generator.publicKey)

  def calcBaseTarget(avgBlockDelay: FiniteDuration, parentHeight: Int, parentBaseTarget: Long,
                     parentTimestamp: Long, maybeGreatGrandParentTimestamp: Option[Long], timestamp: Long): Long = {
    val avgDelayInSeconds = avgBlockDelay.toSeconds

    val prevBaseTarget = parentBaseTarget
    if (parentHeight % 2 == 0) {
      val blocktimeAverage = maybeGreatGrandParentTimestamp.fold(timestamp - parentTimestamp)(ggpts => (timestamp - ggpts) / AvgBlockTimeDepth) / 1000
      val minBlocktimeLimit = normalize(MinBlockDelaySeconds, avgDelayInSeconds)
      val maxBlocktimeLimit = normalize(MaxBlockDelaySeconds, avgDelayInSeconds)
      val baseTargetGamma = normalize(BaseTargetGamma, avgDelayInSeconds)

      val baseTarget = (if (blocktimeAverage > avgDelayInSeconds) {
        prevBaseTarget * Math.min(blocktimeAverage, maxBlocktimeLimit) / avgDelayInSeconds
      } else {
        prevBaseTarget - prevBaseTarget * baseTargetGamma *
          (avgDelayInSeconds - Math.max(blocktimeAverage, minBlocktimeLimit)) / (avgDelayInSeconds * 100)
      }).toLong

      normalizeBaseTarget(baseTarget, avgDelayInSeconds)
    } else {
      prevBaseTarget
    }
  }

  def generatingBalance(state: SnapshotStateReader, fs: FunctionalitySettings, account: Address, atHeight: Int): Long = {
    val generatingBalanceDepth = if (atHeight >= fs.generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    state.effectiveBalance(account, atHeight, generatingBalanceDepth)
  }

  def nextBlockGenerationTime(height: Int, state: SnapshotStateReader, fs: FunctionalitySettings,
                              block: Block, account: PublicKeyAccount, featureProvider: FeatureProvider): Either[String, (Long, Long)] = {
    val balance = generatingBalance(state, fs, account, height)
       Either.cond((!featureProvider.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height) &&balance >= MinimalEffectiveBalanceForGenerator1) ||
          (featureProvider.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height) && balance >= MinimalEffectiveBalanceForGenerator2),
      balance,    s"Balance $balance of ${account.address} is lower than required for generation")
       .flatMap { _ =>
         val cData = block.consensusData
         val hit = calcHit(cData, account)
         val t = cData.baseTarget
         val calculatedTs = (hit * 1000) / (BigInt(t) * balance) + block.timestamp
         if (0 < calculatedTs && calculatedTs < Long.MaxValue) {
           Right((balance, calculatedTs.toLong))
         } else {
           Left(s"Invalid next block generation time: $calculatedTs")
         }
    }
  }

  private def normalizeBaseTarget(bt: Long, averageBlockDelaySeconds: Long): Long = {
    val maxBaseTarget = Long.MaxValue / averageBlockDelaySeconds
    if (bt < MinBaseTarget) MinBaseTarget else if (bt > maxBaseTarget) maxBaseTarget else bt
  }

  private def normalize(value: Long, averageBlockDelaySeconds: Long): Double = value * averageBlockDelaySeconds / (60: Double)

}
