package com.wavesplatform

import kamon.metric.{Histogram, Timer}

import scala.concurrent.{ExecutionContext, Future}

package object metrics {
  final implicit class HistogramExt(val h: Histogram) extends AnyVal {
    def safeRecord(value: Long): Unit = h.record(Math.max(value, 0))
  }

  implicit class TimerExt(val timer: Timer) extends AnyVal {
    def measure[T](f: => T): T = {
      val st     = timer.start()
      val result = f
      st.stop()
      result
    }

    def measureSuccessful[LeftT, RightT](f: => Either[LeftT, RightT]): Either[LeftT, RightT] = {
      val st     = timer.start()
      val result = f
      if (result.isRight) st.stop()
      result
    }

    def measureSuccessful[T](f: => Option[T]): Option[T] = {
      val st     = timer.start()
      val result = f
      if (result.isDefined) st.stop()
      result
    }

    def measureFuture[T](f: => Future[T])(implicit ec: ExecutionContext): Future[T] = {
      val st     = timer.start()
      val future = f
      future.onComplete(_ => st.stop())
      future
    }
  }
}
