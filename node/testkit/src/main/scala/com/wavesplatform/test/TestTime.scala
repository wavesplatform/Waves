package com.wavesplatform.test

import com.wavesplatform.utils.Time

import scala.concurrent.duration.FiniteDuration

case class TestTime(@volatile private var t: Long = System.currentTimeMillis(), private var monotonicMs: Long = System.nanoTime() / 1_000_000)
    extends Time {
  def setTime(tt: Long): this.type = {
    t = tt
    this
  }

  def advance(d: FiniteDuration): this.type = {
    t += d.toMillis
    monotonicMs += d.toMillis
    this
  }

  override def correctedTime(): Long = t

  override def getTimestamp(): Long = {
    t += 1
    t
  }

  override def monotonicMillis(): Long = monotonicMs
}
