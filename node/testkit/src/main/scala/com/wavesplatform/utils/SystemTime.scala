package com.wavesplatform.utils

object SystemTime extends Time {
  def correctedTime(): Long = System.currentTimeMillis()

  @volatile
  private[this] var txTime: Long = 0

  def getTimestamp(): Long = {
    txTime = Math.max(correctedTime(), txTime + 1)
    txTime
  }
}
