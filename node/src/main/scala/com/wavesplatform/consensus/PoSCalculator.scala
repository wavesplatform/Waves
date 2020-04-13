package com.wavesplatform.consensus

import com.wavesplatform.account.{PrivateKey, PublicKey}
import com.wavesplatform.crypto

trait PoSCalculator {
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
  private[consensus] val HitSize: Int        = 8
  private[consensus] val MinBaseTarget: Long = 9

  private[consensus] def generationSignature(signature: Array[Byte], publicKey: PublicKey): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestLength * 2)
    System.arraycopy(signature, 0, s, 0, crypto.DigestLength)
    System.arraycopy(publicKey.arr, 0, s, crypto.DigestLength, crypto.DigestLength)
    crypto.fastHash(s)
  }

  private[consensus] def generationVRFSignature(signature: Array[Byte], privateKey: PrivateKey): Array[Byte] =
    crypto.signVRF(privateKey, signature)

  private[consensus] def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(HitSize).reverse)

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

  import PoSCalculator._

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

  def calculateDelay(hit: BigInt, bt: Long, balance: Long): Long = Math.ceil((BigDecimal(hit) / (BigDecimal(bt) * balance)).toDouble).toLong * 1000

}

class FairPoSCalculator(minBlockTime: Int) extends PoSCalculator {
  import PoSCalculator._

  private[this] val MaxSignature: Array[Byte] = Array.fill[Byte](HitSize)(-1)
  private[this] val MaxHit: BigDecimal        = BigDecimal(BigInt(1, MaxSignature))
  private[this] val C1                        = 70000
  private[this] val C2                        = 5e17

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
    val maxDelay = normalize(90, targetBlockDelaySeconds)
    val minDelay = normalize(30, targetBlockDelaySeconds)

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

object FairPoSCalculator extends FairPoSCalculator(30000) {
  lazy val old = new FairPoSCalculator(5000)
}
