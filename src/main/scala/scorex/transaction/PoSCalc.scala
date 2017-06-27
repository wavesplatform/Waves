package scorex.transaction

import com.wavesplatform.settings.FunctionalitySettings
import com.wavesplatform.state2.ByteStr
import com.wavesplatform.state2.reader.StateReader
import scorex.account.{Account, PublicKeyAccount}
import scorex.block.Block
import scorex.consensus.nxt.NxtLikeConsensusBlockData
import scorex.crypto.hash.FastCryptographicHash
import scorex.crypto.hash.FastCryptographicHash.hash
import scorex.utils.ScorexLogging

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

  private def blockCreationTime(h: History, state: StateReader, fs: FunctionalitySettings)(b: Block) =
    (for {
      parent <- h.blockById(b.reference)
      height <- h.heightOf(b.reference)
      ts <- nextBlockGenerationTime(height, state, fs, parent, b.signerData.generator)
    } yield ts).getOrElse(b.timestamp)

  // This used to be Ordering.by { b => (b.score, -b.timestamp) (overly simplified), but due to
  // https://issues.scala-lang.org/browse/SI-8541, it has been rewritten the way it is now. It also
  // has an added benefit of calculating block generation timestamp only when necessary.
  def blockOrdering(history: History, state: StateReader, fs: FunctionalitySettings): Ordering[Block] = {
    val bct = blockCreationTime(history, state, fs) _
    Ordering.fromLessThan { (b1, b2) =>
      b1.blockScore < b2.blockScore ||
        b1.blockScore == b2.blockScore && bct(b1) < bct(b2)
    }
  }

  def generatingBalance(state: StateReader, fs: FunctionalitySettings)(account: Account, atHeight: Int): Long = {
    val generatingBalanceDepth = if (atHeight >= fs.generatingBalanceDepthFrom50To1000AfterHeight) 1000 else 50
    state.effectiveBalanceAtHeightWithConfirmations(account, atHeight, generatingBalanceDepth)
  }

  def nextBlockGenerationTime(height: Int, state: StateReader, fs: FunctionalitySettings, block: Block, account: PublicKeyAccount): Option[Long] = {
    val balance = generatingBalance(state, fs)(account, height)
    if (balance > MinimalEffectiveBalanceForGenerator) {
      val cData = block.consensusData
      val hit = calcHit(cData, account)
      val t = cData.baseTarget

      val calculatedTs = (hit * 1000) / (BigInt(t) * balance) + block.timestamp
      if (0 < calculatedTs && calculatedTs < Long.MaxValue) {
        Some(calculatedTs.toLong)
      } else {
        log.debug(s"Invalid next block generation time: $calculatedTs")
        None
      }
    } else {
      log.debug(s"Balance $balance of ${ByteStr(account.publicKey)} is lower than $MinimalEffectiveBalanceForGenerator")
      None
    }
  }
}
