package com.wavesplatform.metrics

import com.wavesplatform.utils.LoggerFacade
import kamon.metric.{MeasurementUnit, Metric}

object Instrumented {
  private val NanosInMS = 1000000L

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

  def withTime[R](h: Metric.Histogram, f: => R): (R, Long) = {
    import scala.concurrent.duration.*
    val (result, nanoTime) = withTimeNanos(f)
    h.settings.unit match {
      case u if u == MeasurementUnit.time.nanoseconds =>
        (result, nanoTime)

      case u if u == MeasurementUnit.time.microseconds =>
        (result, nanoTime.nanos.toMicros)

      case u if u == MeasurementUnit.time.seconds =>
        (result, nanoTime.nanos.toSeconds)

      case _ =>
        (result, nanoTime / NanosInMS)
    }
  }
}
