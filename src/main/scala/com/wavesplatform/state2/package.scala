package com.wavesplatform

import cats.{Monad, Traverse}
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{Transaction, ValidationError}
import scorex.utils.ScorexLogging

import scala.util.{Left, Right, Try}
import scala.language.higherKinds

package object state2 {

  def foldM[G[_], F[_], A, B](fa: F[A], z: B)(f: (B, A) => G[B])(implicit G: Monad[G], F: Traverse[F]): G[B] =
    F.foldLeft(fa, G.pure(z))((gb, a) => G.flatMap(gb)(f(_, a)))

  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  implicit class EitherExt[L <: ValidationError, R](ei: Either[L, R]) {
    def liftValidationError[T <: Transaction](t: T): Either[ValidationError, R] = {
      ei.left.map(e => GenericError(e.toString))
    }
  }

  implicit class EitherExt2[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value) => throw new Exception(value.toString)
      case Right(value) => value
    }
  }

  def trim(s: ByteStr): String = s.toString.take(7) + "..."

  trait Instrumented {
    self: ScorexLogging =>
    private def withTime[R](f: => R): (R, Long) = {
      val t0 = System.currentTimeMillis()
      val r: R = f
      val t1 = System.currentTimeMillis()
      (r, t1 - t0)
    }

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
  }
}
