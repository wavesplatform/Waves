package com.wavesplatform

import com.wavesplatform.utils.Time

import scala.concurrent.duration.FiniteDuration

class TestTime(var t: Long = System.currentTimeMillis()) extends Time {
  def setTime(tt: Long): this.type = {
    t = tt
    this
  }

  def advance(d: FiniteDuration): this.type = {
    t += d.toMillis
    this
  }

  override def correctedTime(): Long = t

  override def getTimestamp(): Long = {
    t += 1
    t
  }
}
