package com.wavesplatform.metrics

import kamon.metric.{Histogram, MeasurementUnit}

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

  def withTime[R](h: Histogram, f: => R): (R, Long) = {
    import scala.concurrent.duration._
    val (result, nanoTime) = withTimeNanos(f)
    h.unit match {
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
