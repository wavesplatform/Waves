package com.wavesplatform.metrics

import com.wavesplatform.utils.LoggerFacade

object Instrumented {
  private[this] val NanosInMS = 1000000L

  def withTimeNanos[R](f: => R): (R, Long) = {
    val startTime = System.nanoTime()
    val result: R = f
    val endTime   = System.nanoTime()
    (result, endTime - startTime)
  }

  def withTimeMillis[R](f: => R): (R, Long) = {
    val (result, nanos) = withTimeNanos(f)
    (result, nanos / NanosInMS)
  }

  def logMeasure[R](log: LoggerFacade, action: String)(f: => R): R = {
    val (result, time) = Instrumented.withTimeMillis(f)
    log.trace(s"$action took ${time}ms")
    result
  }
}
