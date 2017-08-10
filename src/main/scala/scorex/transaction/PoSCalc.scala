package scorex.transaction

import com.google.common.base.Throwables
import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Address, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.hash.FastCryptographicHash
import scorex.crypto.hash.FastCryptographicHash.hash
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success, Try}

object PoSCalc extends ScorexLogging {

  val MinimalEffectiveBalanceForGenerator: Long = 1000000000000L
  val AvgBlockTimeDepth: Int = 3

  def calcTarget(prevBlock: Block, timestamp: Long, balance: Long): BigInt = {
    val eta = (timestamp - prevBlock.timestamp) / 1000
    BigInt(prevBlock.consensusData.baseTarget) * eta * balance
  }

  def calcHit(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): BigInt =
    BigInt(1, calcGeneratorSignature(lastBlockData, generator).take(8).reverse)

  def calcGeneratorSignature(lastBlockData: NxtLikeConsensusBlockData, generator: PublicKeyAccount): FastCryptographicHash.Digest =
    hash(lastBlockData.generationSignature ++ generator.publicKey)

  def calcBaseTarget(avgBlockDelay: FiniteDuration, parentHeight: Int, parent: Block, greatGrandParent: Option[Block], timestamp: Long): Long = {
    val avgDelayInSeconds = avgBlockDelay.toSeconds

    def normalize(value: Long): Double = value * avgDelayInSeconds / (60: Double)

    val prevBaseTarget = parent.consensusData.baseTarget
    if (parentHeight % 2 == 0) {
      val blocktimeAverage = greatGrandParent.fold(timestamp - parent.timestamp) {
        b => (timestamp - b.timestamp) / AvgBlockTimeDepth
      } / 1000

      val minBlocktimeLimit = normalize(53)
      val maxBlocktimeLimit = normalize(67)
      val baseTargetGamma = normalize(64)
      val maxBaseTarget = Long.MaxValue / avgDelayInSeconds

      val baseTarget = (if (blocktimeAverage > avgDelayInSeconds) {
        prevBaseTarget * Math.min(blocktimeAverage, maxBlocktimeLimit) / avgDelayInSeconds
      } else {
        prevBaseTarget - prevBaseTarget * baseTargetGamma *
          (avgDelayInSeconds - Math.max(blocktimeAverage, minBlocktimeLimit)) / (avgDelayInSeconds * 100)
      }).toLong

      scala.math.min(baseTarget, maxBaseTarget)
    } else {
      prevBaseTarget
    }
  }

  def generatingBalance(state: StateReader, fs: FunctionalitySettings, account: Address, atHeight: Int): Try[Long] = {
    val generatingBalanceDepth = if (atHeight >= fs.generationBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, generatingBalanceDepth)
  }

  def nextBlockGenerationTime(height: Int, state: StateReader, fs: FunctionalitySettings,
                              block: Block, account: PublicKeyAccount): Either[String, Long] = {
    generatingBalance(state, fs, account, height) match {
      case Success(balance) => for {
        _ <- Either.cond(balance >= MinimalEffectiveBalanceForGenerator, (),
          s"Balance $balance of ${account.address} is lower than $MinimalEffectiveBalanceForGenerator")
        cData = block.consensusData
        hit = calcHit(cData, account)
        t = cData.baseTarget
        calculatedTs = (hit * 1000) / (BigInt(t) * balance) + block.timestamp
        _ <- Either.cond(0 < calculatedTs && calculatedTs < Long.MaxValue, (), s"Invalid next block generation time: $calculatedTs")
      } yield calculatedTs.toLong
      case Failure(exc) =>
        log.error("Critical error calculating nextBlockGenerationTime", exc)
        Left(Throwables.getStackTraceAsString(exc))
    }
  }

}
