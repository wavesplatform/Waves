package com.wavesplatform.test

import com.wavesplatform.utils.Time

import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class TestTime(@volatile private var t: Long = System.currentTimeMillis(), private var monotonicMs: Long = System.nanoTime() / 1_000_000)
    extends Time {
  def setTime(tt: Long): this.type = {
    t = tt
    this
  }

  def setTimeIfGreater(tt: Long): this.type = {
    if (tt > t) {
      monotonicMs += tt - t
      t = tt
    }
    this
  }

  def advance(d: FiniteDuration): this.type = if (d <= 0.millis) this
  else {
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
