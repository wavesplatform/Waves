package com.wavesplatform

import java.util.concurrent.TimeUnit
import kamon.Kamon
import kamon.metric.Timer

package object metrics {
  final implicit class TimerExt(private val timer: Timer) extends AnyVal {
    def measure[T](f: => T): T =
      measureWithFilter(f)(_ => true)

    def measureSuccessful[LeftT, RightT](f: => Either[LeftT, RightT]): Either[LeftT, RightT] =
      measureWithFilter(f)(_.isRight)

    def measureOptional[T](f: => Option[T]): Option[T] =
      measureWithFilter(f)(_.isDefined)

    private def measureWithFilter[T](f: => T)(filter: T => Boolean): T =
      if (!Kamon.enabled()) f
      else {
        val start   = System.nanoTime()
        val result  = f
        val elapsed = System.nanoTime() - start
        if (elapsed > 0 && filter(result)) {
          timer.record(elapsed, TimeUnit.NANOSECONDS)
        }
        result
      }
  }
}
