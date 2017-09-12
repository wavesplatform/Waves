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
//  case class Measured[T](x: T, measurements: Map[String, Long])
//
//  object Measured {
//    implicit def monad[T]: Monad[Measured[T]] = new Monad[Measured[T]] {
//      override def pure[A](x: A) = Measured(x, Map.empty)
//      override def flatMap[A, B](fa: Measured[A])(f: (A) => Measured[B]): Measured[B] = {
//        val fb = f(fa.x)
//        Measured(fb.x, Monoid.combine(fa.measurements, fb.measurements))
//      }
//      override def tailRecM[A, B](a: A)(f: (A) => Measured[Either[A, B]]) = {
//
//      }
//    }
//
//    def apply[T](name: String)(f: => T): Measured[T] = {
//      val start = System.currentTimeMillis()
//      val v = f
//      val end = System.currentTimeMillis()
//      Measured[T](v, Map(name -> (end - start)))
//    }
//  }

  def withTime[R](f: => R): (R, Long) = {
    val t0 = System.currentTimeMillis()
    val r: R = f
    val t1 = System.currentTimeMillis()
    (r, t1 - t0)
  }
}
