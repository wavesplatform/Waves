package com.wavesplatform

import kamon.metric.Metric

package object metrics {
  final implicit class TimerExt(private val timer: Metric.Timer) extends AnyVal {
    def measure[T](f: => T): T =
      measureWithFilter(f)(_ => true)

    def measureSuccessful[LeftT, RightT](f: => Either[LeftT, RightT]): Either[LeftT, RightT] =
      measureWithFilter(f)(_.isRight)

    def measureOptional[T](f: => Option[T]): Option[T] =
      measureWithFilter(f)(_.isDefined)

    private[this] def measureWithFilter[T](f: => T)(filter: T => Boolean): T = {
      val startedTimer = timer.withoutTags().start()
      val result       = f
      if (filter(result)) startedTimer.stop()
      result
    }
  }
}
