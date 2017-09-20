package scorex.transaction

import com.wavesplatform.features.Functionalities
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Address, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.hash.FastCryptographicHash
import scorex.crypto.hash.FastCryptographicHash.hash
import scorex.utils.ScorexLogging

import scala.concurrent.duration.FiniteDuration

object PoSCalc extends ScorexLogging {

  private val shallowGenerationBalanceDepth = 50
  private val deepGenerationBalanceDepth = 1000

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

  def generatingBalance(state: StateReader, fn: Functionalities, account: Address, atHeight: Int): Long = {
    val generatingBalanceDepth = fn.deepGenerationBalanceDepthAfter.check(atHeight)
      .fold(_ => deepGenerationBalanceDepth, _ => shallowGenerationBalanceDepth)
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, generatingBalanceDepth)
  }

  def nextBlockGenerationTime(
                               height: Int,
                               state: StateReader,
                               fn: Functionalities,
                               block: Block,
                               account: PublicKeyAccount): Either[String, Long] = {
    val balance = generatingBalance(state, fn, account, height)
    Either.cond(balance >= MinimalEffectiveBalanceForGenerator,
      balance,
      s"Balance $balance of ${account.address} is lower than $MinimalEffectiveBalanceForGenerator")
      .flatMap { _ =>
        val cData = block.consensusData
        val hit = calcHit(cData, account)
        val t = cData.baseTarget

        val calculatedTs = (hit * 1000) / (BigInt(t) * balance) + block.timestamp
        if (0 < calculatedTs && calculatedTs < Long.MaxValue) {
          Right(calculatedTs.toLong)
        } else {
          Left(s"Invalid next block generation time: $calculatedTs")
        }
      }
  }
}
