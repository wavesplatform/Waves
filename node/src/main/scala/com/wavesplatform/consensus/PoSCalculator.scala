package com.wavesplatform.consensus

import com.wavesplatform.account.{PrivateKey, PublicKey}
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.consensus.PoSCalculator.HitSize
import com.wavesplatform.crypto
import com.wavesplatform.settings.FunctionalitySettings

sealed trait PoSCalculator {
  def calculateBaseTarget(
      targetBlockDelaySeconds: Long,
      prevHeight: Int,
      prevBaseTarget: Long,
      parentTimestamp: Long,
      maybeGreatGrandParentTimestamp: Option[Long],
      timestamp: Long
  ): Long

  def calculateDelay(hit: BigInt, bt: Long, balance: Long): Long
}

object PoSCalculator {
  val HitSize: Int        = 8
  val MinBaseTarget: Long = 9

  def generationSignature(signature: ByteStr, publicKey: PublicKey): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestLength * 2)
    System.arraycopy(signature.arr, 0, s, 0, crypto.DigestLength)
    System.arraycopy(publicKey.arr, 0, s, crypto.DigestLength, crypto.DigestLength)
    crypto.fastHash(s)
  }

  private[consensus] def generationVRFSignature(signature: Array[Byte], privateKey: PrivateKey): ByteStr =
    crypto.signVRF(privateKey, signature)

  def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(HitSize).reverse)

  private[consensus] def normalize(value: Long, targetBlockDelaySeconds: Long): Double =
    value * targetBlockDelaySeconds / (60: Double)

  private[consensus] def normalizeBaseTarget(baseTarget: Long, targetBlockDelaySeconds: Long): Long = {
    baseTarget
      .max(MinBaseTarget)
      .min(Long.MaxValue / targetBlockDelaySeconds)
  }
}

object NxtPoSCalculator extends PoSCalculator {
  protected val MinBlockDelaySeconds = 53
  protected val MaxBlockDelaySeconds = 67
  protected val BaseTargetGamma      = 64
  protected val MeanCalculationDepth = 3

  import PoSCalculator.*

  def calculateBaseTarget(
      targetBlockDelaySeconds: Long,
      prevHeight: Int,
      prevBaseTarget: Long,
      parentTimestamp: Long,
      maybeGreatGrandParentTimestamp: Option[Long],
      timestamp: Long
  ): Long = {

    if (prevHeight % 2 == 0) {
      val meanBlockDelay  = maybeGreatGrandParentTimestamp.fold(timestamp - parentTimestamp)(ts => (timestamp - ts) / MeanCalculationDepth) / 1000
      val minBlockDelay   = normalize(MinBlockDelaySeconds, targetBlockDelaySeconds)
      val maxBlockDelay   = normalize(MaxBlockDelaySeconds, targetBlockDelaySeconds)
      val baseTargetGamma = normalize(BaseTargetGamma, targetBlockDelaySeconds)

      val baseTarget = (if (meanBlockDelay > targetBlockDelaySeconds) {
                          prevBaseTarget * Math.min(meanBlockDelay.toDouble, maxBlockDelay) / targetBlockDelaySeconds
                        } else {
                          prevBaseTarget - prevBaseTarget * baseTargetGamma *
                            (targetBlockDelaySeconds - Math.max(meanBlockDelay.toDouble, minBlockDelay)) / (targetBlockDelaySeconds * 100)
                        }).toLong

      normalizeBaseTarget(baseTarget, targetBlockDelaySeconds)
    } else {
      prevBaseTarget
    }
  }

  def calculateDelay(hit: BigInt, bt: Long, balance: Long): Long = Math.ceil((BigDecimal(hit) / (BigDecimal(bt) * balance)).toDouble).toLong * 1000
}

object FairPoSCalculator {
  lazy val V1: FairPoSCalculator = FairPoSCalculator(5000, 0)
  lazy val V2: FairPoSCalculator = FairPoSCalculator(15000, 8)

  def fromSettings(fs: FunctionalitySettings): PoSCalculator =
    if (fs.minBlockTime.toSeconds == 15 && fs.delayDelta == 8) V2
    else FairPoSCalculator(fs.minBlockTime.toMillis.toInt, fs.delayDelta)

  val MaxHit     = BigDecimal(BigInt(1, Array.fill[Byte](HitSize)(-1)))
  private val C1 = 70000
  private val C2 = 5e17
}

case class FairPoSCalculator(minBlockTime: Int, delayDelta: Int) extends PoSCalculator {
  import FairPoSCalculator.*
  import PoSCalculator.*

  def calculateDelay(hit: BigInt, bt: Long, balance: Long): Long = {
    val h = (BigDecimal(hit) / MaxHit).toDouble
    val a = minBlockTime + C1 * math.log(1 - C2 * math.log(h) / bt / balance)
    a.toLong
  }

  def calculateBaseTarget(
      targetBlockDelaySeconds: Long,
      prevHeight: Int,
      prevBaseTarget: Long,
      parentTimestamp: Long,
      maybeGreatGrandParentTimestamp: Option[Long],
      timestamp: Long
  ): Long = {
    val maxDelay = normalize(90 - delayDelta, targetBlockDelaySeconds)
    val minDelay = normalize(30 + delayDelta, targetBlockDelaySeconds)

    maybeGreatGrandParentTimestamp match {
      case None =>
        prevBaseTarget
      case Some(ts) =>
        val avg = (timestamp - ts) / 3 / 1000
        if (avg > maxDelay) prevBaseTarget + math.max(1, prevBaseTarget / 100)
        else if (avg < minDelay) prevBaseTarget - math.max(1, prevBaseTarget / 100)
        else prevBaseTarget
    }
  }
}
