package scorex.transaction

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Account, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.hash.FastCryptographicHash
import scorex.crypto.hash.FastCryptographicHash.hash
import scorex.utils.{ScorexLogging, Time}

import scala.concurrent.duration.FiniteDuration

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

  def calcBaseTarget(history: History)(avgBlockDelay: FiniteDuration, prevBlock: Block, timestamp: Long): Long = {
    val avgDelayInSeconds = avgBlockDelay.toSeconds

    def normalize(value: Long): Double = value * avgDelayInSeconds / (60: Double)

    val height = history.heightOf(prevBlock).get
    val prevBaseTarget = prevBlock.consensusData.baseTarget
    if (height % 2 == 0) {
      val blocktimeAverage = history.parent(prevBlock, AvgBlockTimeDepth - 1)
        .map(b => (timestamp - b.timestamp) / AvgBlockTimeDepth)
        .getOrElse(timestamp - prevBlock.timestamp) / 1000

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

  def blockOrdering(history: History, state: StateReader, fs: FunctionalitySettings, time: Time): Ordering[Block] = Ordering.by {
    block =>
      val parent = history.blockById(block.reference).get
      val blockCreationTime = nextBlockGenerationTime(history, state, fs, time: Time)(parent, block.signerData.generator)
        .getOrElse(block.timestamp)
      (block.blockScore, -blockCreationTime)
  }

  def generatingBalance(state: StateReader, fs: FunctionalitySettings)(account: Account, atHeight: Int): Long = {
    val generatingBalanceDepth = if (atHeight >= fs.generatingBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, generatingBalanceDepth)
  }

  def nextBlockGenerationTime(history: History, state: StateReader, fs: FunctionalitySettings, time: Time)(block: Block, account: PublicKeyAccount): Option[Long] = {
    history.heightOf(block.uniqueId)
      .map(height => (height, generatingBalance(state, fs)(account, height))).filter(_._2 > 0)
      .flatMap {
        case (height, balance) =>
          val cData = block.consensusData
          val hit = calcHit(cData, account)
          val t = cData.baseTarget

          val result =
            Some((hit * 1000) / (BigInt(t) * balance) + block.timestamp)
              .filter(_ > 0).filter(_ < Long.MaxValue)
              .map(_.toLong)

          log.debug({
            val currentTime = time.correctedTime()
            s"Next block gen time: $result " +
              s"in ${
                result.map(t => (t - currentTime) / 1000)
              } seconds, " +
              s"hit: $hit, target: $t, " +
              s"account:  $account, account balance: $balance " +
              s"last block id: ${
                block.encodedId
              }, " +
              s"height: $height"
          })

          result
      }
  }

}
