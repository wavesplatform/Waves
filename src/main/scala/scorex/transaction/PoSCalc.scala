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

trait PoSCalculator {
  protected val HitSize: Int = 8
  protected val MeanCalculationDepth: Int = 3

  // Min BaseTarget value is 9 because only in this case it is possible to get to next integer value (10)
  // then increasing base target by 11% and casting it to Long afterward (see lines 55 and 59)
  private val MinBaseTarget: Long = 9

  private val MinBlockDelaySeconds = 53
  private val MaxBlockDelaySeconds = 67
  private val BaseTargetGamma = 64


  def generatorSignature(signature: Array[Byte], publicKey: Array[Byte]): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestSize * 2)
    System.arraycopy(signature, 0, s, 0, crypto.DigestSize)
    System.arraycopy(publicKey, 0, s, crypto.DigestSize, crypto.DigestSize)
    crypto.fastHash(s)
  }

  def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(HitSize).reverse)

  def baseTarget(targetBlockDelaySeconds: Int, prevHeight: Int, prevBaseTarget: Long, parentTimestamp: Long,
                 maybeGreatGrandParentTimestamp: Option[Long], timestamp: Long): Long = {

    if (prevHeight % 2 == 0) {
      val meanBlockDelay = maybeGreatGrandParentTimestamp.fold(timestamp - parentTimestamp)(ts => (timestamp - ts) / MeanCalculationDepth) / 1000
      val minBlockDelay = normalize(MinBlockDelaySeconds, targetBlockDelaySeconds)
      val maxBlockDelay = normalize(MaxBlockDelaySeconds, targetBlockDelaySeconds)
      val baseTargetGamma = normalize(BaseTargetGamma, targetBlockDelaySeconds)

      val baseTarget = (if (meanBlockDelay > targetBlockDelaySeconds) {
        prevBaseTarget * Math.min(meanBlockDelay, maxBlockDelay) / targetBlockDelaySeconds
      } else {
        prevBaseTarget - prevBaseTarget * baseTargetGamma *
          (targetBlockDelaySeconds - Math.max(meanBlockDelay, minBlockDelay)) / (targetBlockDelaySeconds * 100)
      }).toLong

      normalizeBaseTarget(baseTarget, targetBlockDelaySeconds)
    } else {
      prevBaseTarget
    }

  }

  def target(prevBlockTimestamp: Long, prevBaseTarget: Long, timestamp: Long, balance: Long): BigInt = {
    val blockDelaySeconds = (timestamp - prevBlockTimestamp) / 1000
    BigInt(prevBaseTarget) * blockDelaySeconds * balance
  }

  def time(hit: BigInt, bt: Long, balance: Long): Long = ((hit * 1000) / (BigInt(bt) * balance)).toLong

  def nextBlockGenerationTime(height: Int, state: SnapshotStateReader, fs: FunctionalitySettings, block: Block,
                              account: PublicKeyAccount, featureProvider: FeatureProvider): Either[String, (Long, Long)] = {
    generatingBalance(state(), fs, account, height) match {
      case Success(balance) => for {
        _ <- Either.cond((!featureProvider.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height) && balance >= MinimalEffectiveBalanceForGenerator1) ||
          (featureProvider.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height) && balance >= MinimalEffectiveBalanceForGenerator2), (),
          s"Balance $balance of ${account.address} is lower than required for generation")
        cData = block.consensusData
        hit = calcHit(cData, account)
        t = cData.baseTarget
        calculatedTs = (hit * 1000) / (BigInt(t) * balance) + block.timestamp
        _ <- Either.cond(0 < calculatedTs && calculatedTs < Long.MaxValue, (), s"Invalid next block generation time: $calculatedTs")
      } yield (balance, calculatedTs.toLong)
      case Failure(exc) =>
        log.error("Critical error calculating nextBlockGenerationTime", exc)
        Left(Throwables.getStackTraceAsString(exc))
    }
  }

  private def normalize(value: Long, targetBlockDelaySeconds: Long): Double =
    value * targetBlockDelaySeconds / (60: Double)

  private def normalizeBaseTarget(baseTarget: Long, targetBlockDelaySeconds: Long): Long = {
    val maxBaseTarget = Long.MaxValue / targetBlockDelaySeconds
    if (baseTarget < MinBaseTarget) MinBaseTarget else if (baseTarget > maxBaseTarget) maxBaseTarget else baseTarget
  }

}

object NxtPoSCalculator extends PoSCalculator

object FairPoSCalculator extends PoSCalculator {
  private val MaxSignature: Array[Byte] = Array.fill[Byte](HitSize)(-1)
  private val MaxHit: Double = BigInt(1, MaxSignature).toDouble
  //  private val Correction: Double = MaxHit / log2(MaxHit)

  override def hit(generatorSignature: Array[Byte]): BigInt = {
    val h = super.hit(generatorSignature)
    val x: Double = h.toDouble / MaxHit
    val y = math.abs(log2(x))
    BigDecimal(y).setScale(0, BigDecimal.RoundingMode.CEILING).toBigInt()
  }

  private def log2(x: BigDecimal): Double = math.log10(x.toDouble) / math.log10(2.0)

}

object GeneratingBalanceProvider {
  val MinimalEffectiveBalanceForGenerator1: Long = 1000000000000L
  val MinimalEffectiveBalanceForGenerator2: Long = 100000000000L

  private val FirstDepth = 50
  private val SecondDepth = 1000

  def balance(state: SnapshotStateReader, fs: FunctionalitySettings, account: Address, height: Int): Long = {
    val depth = if (height >= fs.generationBalanceDepthFrom50To1000AfterHeight) SecondDepth else FirstDepth
    state.effectiveBalance(account, height, depth)
  }
}

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
    Either.cond((!featureProvider.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height) && balance >= MinimalEffectiveBalanceForGenerator1) ||
      (featureProvider.isFeatureActivated(BlockchainFeatures.SmallerMinimalGeneratingBalance, height) && balance >= MinimalEffectiveBalanceForGenerator2),
      balance, s"Balance $balance of ${account.address} is lower than required for generation")
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
