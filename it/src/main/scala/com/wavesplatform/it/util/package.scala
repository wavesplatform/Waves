package com.wavesplatform.it

import com.wavesplatform.settings.Constants
import io.netty.util.Timer

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

package object util {
  implicit class TimerExt(val timer: Timer) extends AnyVal {
    def schedule[A](f: => Future[A], delay: FiniteDuration): Future[A] = {
      val p = Promise[A]
      try {
        timer.newTimeout(_ => p.completeWith(f), delay.length, delay.unit)
      } catch {
        case NonFatal(e) => p.failure(e)
      }
      p.future
    }

    def sleep(term: FiniteDuration): Future[Unit] = schedule(Future.successful(()), term)

    def retryUntil[A](f: => Future[A], cond: A => Boolean, retryInterval: FiniteDuration)(implicit ec: ExecutionContext): Future[A] =
      f.flatMap(v => if (cond(v)) Future.successful(v) else schedule(retryUntil(f, cond, retryInterval), retryInterval))
  }
  implicit class DoubleExt(val d: Double) extends AnyVal {
    def waves: Long = (BigDecimal(d) * Constants.UnitsInWave).toLong
  }
}
