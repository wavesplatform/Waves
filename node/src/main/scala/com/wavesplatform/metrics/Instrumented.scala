package com.wavesplatform.metrics

import com.wavesplatform.utils.ScorexLogging
import kamon.metric.{Counter, Histogram}

trait Instrumented {
  self: ScorexLogging =>

  import Instrumented._

  def measureSizeLog[F[_] <: TraversableOnce[_], A, R](s: String)(fa: => F[A])(f: F[A] => R): R = {
    val (r, time) = withTime(f(fa))
    log.trace(s"processing of ${fa.size} $s took ${time}ms")
    r
  }

  def measureLog[R](s: String)(f: => R): R = {
    val (r, time) = withTime(f)
    log.trace(s"$s took ${time}ms")
    r
  }

  def measureSuccessful[A, B](h: Histogram, f: => Either[A, B]): Either[A, B] = {
    val (r, time) = withTime(f)
    if (r.isRight)
      h.safeRecord(time)
    r
  }

  def incrementSuccessful[A, B](c: Counter, f: => Either[A, B]): Either[A, B] = {
    val r = f
    if (r.isRight)
      c.increment()
    r
  }

  def measureAndIncSuccessful[A, B](h: Histogram, c: Counter)(f: => Either[A, B]): Either[A, B] = {
    measureSuccessful(h, incrementSuccessful(c, f))
  }

  def measureSuccessful[A](h: Histogram, f: => Option[A]): Option[A] = {
    val (r, time) = withTime(f)
    if (r.isDefined)
      h.safeRecord(time)
    r
  }

  def measureSuccessfulFun[A, B](writeTime: Long => Unit, f: => Either[A, B]): Either[A, B] = {
    val (r, time) = withTime(f)
    if (r.isRight)
      writeTime(time)
    r
  }

  def measureSuccessfulFun[A](writeTime: Long => Unit, f: => Option[A]): Option[A] = {
    val (r, time) = withTime(f)
    if (r.isDefined)
      writeTime(time)
    r
  }
}

object Instrumented {

  def withTime[R](f: => R): (R, Long) = {
    val t0   = System.currentTimeMillis()
    val r: R = f
    val t1   = System.currentTimeMillis()
    (r, t1 - t0)
  }

  def measure[R](h: Histogram)(f: => R): R = {
    val (r, time) = withTime(f)
    h.record(time)
    r
  }

}
