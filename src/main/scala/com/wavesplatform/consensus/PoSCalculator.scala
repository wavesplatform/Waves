package com.wavesplatform.consensus

import com.wavesplatform.crypto

trait PoSCalculator {
  protected val HitSize: Int        = 8
  protected val MinBaseTarget: Long = 9

  def generatorSignature(signature: Array[Byte], publicKey: Array[Byte]): Array[Byte] = {
    val s = new Array[Byte](crypto.DigestSize * 2)
    System.arraycopy(signature, 0, s, 0, crypto.DigestSize)
    System.arraycopy(publicKey, 0, s, crypto.DigestSize, crypto.DigestSize)
    crypto.fastHash(s)
  }

  def baseTarget(targetBlockDelaySeconds: Long,
                 prevHeight: Int,
                 prevBaseTarget: Long,
                 parentTimestamp: Long,
                 maybeGreatGrandParentTimestamp: Option[Long],
                 timestamp: Long): Long

  def hit(generatorSignature: Array[Byte]): BigInt = BigInt(1, generatorSignature.take(HitSize).reverse)

  def calculateDelay(hit: BigInt, bt: Long, balance: Long): Long

  def validBlockDelay(genSig: Array[Byte], baseTarget: Long, balance: Long): Long = {
    calculateDelay(hit(genSig), baseTarget, balance)
  }

  def validBlockDelay(genSig: Array[Byte], publicKey: Array[Byte], baseTarget: Long, balance: Long): Long = {
    calculateDelay(hit(generatorSignature(genSig, publicKey)), baseTarget, balance)
  }

  def normalize(value: Long, targetBlockDelaySeconds: Long): Double =
    value * targetBlockDelaySeconds / (60: Double)

  def normalizeBaseTarget(baseTarget: Long, targetBlockDelaySeconds: Long): Long = {
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

  def baseTarget(targetBlockDelaySeconds: Long,
                 prevHeight: Int,
                 prevBaseTarget: Long,
                 parentTimestamp: Long,
                 maybeGreatGrandParentTimestamp: Option[Long],
                 timestamp: Long): Long = {

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

  def calculateDelay(hit: BigInt, bt: Long, balance: Long): Long = ((hit * 1000) / (BigInt(bt) * balance)).toLong

}

object FairPoSCalculator extends PoSCalculator {
  private val MaxSignature: Array[Byte] = Array.fill[Byte](HitSize)(-1)
  private val MaxHit: BigDecimal        = BigDecimal(BigInt(1, MaxSignature))
  private val C1                        = 70000
  private val C2                        = 5E17
  private val TMin                      = 5000

  def calculateDelay(hit: BigInt, bt: Long, balance: Long): Long = {
    val h = (BigDecimal(hit) / MaxHit).toDouble
    val a = TMin + C1 * math.log(1 - C2 * math.log(h) / bt / balance)
    a.toLong
  }

  def baseTarget(targetBlockDelaySeconds: Long,
                 prevHeight: Int,
                 prevBaseTarget: Long,
                 parentTimestamp: Long,
                 maybeGreatGrandParentTimestamp: Option[Long],
                 timestamp: Long): Long = {
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
