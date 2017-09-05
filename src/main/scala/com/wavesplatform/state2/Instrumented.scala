package com.wavesplatform.state2

import kamon.metric.instrument.Histogram
import scorex.utils.ScorexLogging

import scala.language.higherKinds

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
      h.record(time)
    r
  }

  def measureSuccessful[A](h: Histogram, f: => Option[A]): Option[A] = {
    val (r, time) = withTime(f)
    if (r.isDefined)
      h.record(time)
    r
  }
}

object Instrumented {
  def withTime[R](f: => R): (R, Long) = {
    val t0 = System.currentTimeMillis()
    val r: R = f
    val t1 = System.currentTimeMillis()
    (r, t1 - t0)
  }
}
