package com.wavesplatform
import com.wavesplatform.utils.Time
import org.scalatest.Suite

trait NTPTime { _: Suite =>
  protected val ntpTime: Time = new Time {
    def correctedTime(): Long = System.currentTimeMillis()

    private var txTime: Long = 0

    def getTimestamp(): Long = {
      txTime = Math.max(correctedTime(), txTime + 1)
      txTime
    }
  }

  protected def ntpNow: Long = ntpTime.getTimestamp()
}
