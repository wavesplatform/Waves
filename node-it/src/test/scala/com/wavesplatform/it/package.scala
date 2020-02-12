package com.wavesplatform

import com.wavesplatform.state.DataEntry
import io.netty.util.Timer

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

package object it {
  implicit val ec: ExecutionContext = ExecutionContext.Implicits.global

  //TODO для чего?
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
    private def toSatoshi(decimals: Int): Long = (BigDecimal(d) * Math.pow(10, decimals)).toLong
    def tokenD0: Long = toSatoshi(0)
    def tokenD1: Long = toSatoshi(1)
    def tokenD2: Long = toSatoshi(2)
    def tokenD3: Long = toSatoshi(3)
    def tokenD4: Long = toSatoshi(4)
    def tokenD5: Long = toSatoshi(5)
    def tokenD6: Long = toSatoshi(6)
    def tokenD7: Long = toSatoshi(7)
    def tokenD8: Long = toSatoshi(8)
    def waves: Long = tokenD8
  }

  //TODO для чего?
  implicit class TypedDataEntry(entry: DataEntry[_]) {
    def as[T]: T = entry.asInstanceOf[T]
  }
}
