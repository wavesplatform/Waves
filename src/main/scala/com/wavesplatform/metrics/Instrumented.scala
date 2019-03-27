package com.wavesplatform.metrics

import com.wavesplatform.utils.ScorexLogging
import kamon.metric.{Counter, Histogram, MeasurementUnit}

trait Instrumented { self: ScorexLogging =>
  import Instrumented._

  def measureSizeLog[F[_] <: TraversableOnce[_], A, R](s: String)(fa: => F[A])(f: F[A] => R): R = {
    val (result, time) = withTimeMillis(f(fa))
    log.trace(s"processing of ${fa.size} $s took ${time}ms")
    result
  }

  def measureLog[R](s: String)(f: => R): R = {
    val (result, time) = withTimeMillis(f)
    log.trace(s"$s took ${time}ms")
    result
  }

  def measureSuccessful[A, B](h: Histogram, f: => Either[A, B]): Either[A, B] = {
    val (result, time) = withTime(h, f)
    if (result.isRight) h.record(time)
    result
  }

  def incrementSuccessful[A, B](c: Counter, f: => Either[A, B]): Either[A, B] = {
    val result = f
    if (result.isRight) c.increment()
    result
  }

  def measureAndIncSuccessful[A, B](h: Histogram, c: Counter)(f: => Either[A, B]): Either[A, B] = {
    measureSuccessful(h, incrementSuccessful(c, f))
  }

  def measureSuccessful[A](h: Histogram, f: => Option[A]): Option[A] = {
    val (result, time) = withTime(h, f)
    if (result.isDefined) h.record(time)
    result
  }

  def measureSuccessfulFun[A, B](writeTime: Long => Unit, f: => Either[A, B]): Either[A, B] = {
    val (result, time) = withTimeMillis(f)
    if (result.isRight) writeTime(time)
    result
  }

  def measureSuccessfulFun[A](writeTime: Long => Unit, f: => Option[A]): Option[A] = {
    val (result, time) = withTimeMillis(f)
    if (result.isDefined) writeTime(time)
    result
  }
}

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

  def measure[R](h: Histogram)(f: => R): R = {
    val (result, time) = withTime(h, f)
    h.record(time)
    result
  }
}
