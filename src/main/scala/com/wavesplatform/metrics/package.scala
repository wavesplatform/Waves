package com.wavesplatform

import kamon.metric.{Histogram, Timer}

import scala.concurrent.{ExecutionContext, Future}

package object metrics {
  final implicit class HistogramExt(val h: Histogram) extends AnyVal {
    def safeRecord(value: Long): Unit = h.record(Math.max(value, 0))
  }

  implicit class TimerExt(val timer: Timer) extends AnyVal {
    def measure[A](f: => A): A = {
      val st     = timer.start()
      val result = f
      st.stop()
      result
    }

    def measureFuture[A](f: => Future[A])(implicit ec: ExecutionContext): Future[A] = {
      val st     = timer.start()
      val future = f
      future.onComplete(_ => st.stop())
      future
    }
  }
}
