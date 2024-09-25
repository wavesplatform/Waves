package com.wavesplatform.common

import scala.util.{Failure, Success, Try}

package object utils {
  val Base58Alphabet = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

  implicit class EitherExt2[A, B](val ei: Either[A, B]) extends AnyVal {
    def explicitGet(): B = ei match {
      case Left(value)  => throw makeException(value)
      case Right(value) => value
    }

    // used for destructuring in for-comprehensions
    def withFilter(check: B => Boolean): Either[A, B] =
      ei.filterOrElse(check, throw new MatchError(ei))

    def foldToTry: Try[B] = ei.fold(
      left => Failure(makeException(left)),
      right => Success(right)
    )

    @inline
    private def makeException(value: Any): Throwable = value match {
      case err: Throwable => err
      case _              => new RuntimeException(value.toString)
    }
  }
}
