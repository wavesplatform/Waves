package com.wavesplatform.common

import scala.util.{Failure, Success, Try}

package object utils {
  implicit class EitherExt2[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value)  => throw makeException(value)
      case Right(value) => value
    }

    def foldToTry: Try[B] = ei.fold(
      left => Failure(makeException(left)),
      right => Success(right)
    )

    @inline
    private[this] def makeException(value: Any): Throwable = value match {
      case err: Throwable => err
      case _              => new RuntimeException(value.toString)
    }
  }
}
