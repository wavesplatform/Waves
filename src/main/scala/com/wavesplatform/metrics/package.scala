package com.wavesplatform

import kamon.metric.{Histogram, Timer}

import scala.concurrent.{ExecutionContext, Future}

package object metrics {
  final implicit class HistogramExt(private val histogram: Histogram) extends AnyVal {
    def safeRecord(value: Long): Unit = histogram.record(Math.max(value, 0L))
  }

  final implicit class TimerExt(private val timer: Timer) extends AnyVal {
    def measure[T](f: => T): T = {
      val startedTimer = timer.start()
      val result       = f
      startedTimer.stop()
      result
    }

    def measureSuccessful[LeftT, RightT](f: => Either[LeftT, RightT]): Either[LeftT, RightT] = {
      val startedTimer = timer.start()
      val result       = f
      if (result.isRight) startedTimer.stop()
      result
    }

    def measureSuccessful[T](f: => Option[T]): Option[T] = {
      val startedTimer = timer.start()
      val result       = f
      if (result.isDefined) startedTimer.stop()
      result
    }

    def measureFuture[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
      val startedTimer = timer.start()
      val future       = f
      future.onComplete(_ => startedTimer.stop())
      future
    }
  }
}
