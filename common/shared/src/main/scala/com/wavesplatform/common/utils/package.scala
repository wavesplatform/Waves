package com.wavesplatform.common
import scala.util.{Failure, Success, Try}

package object utils {
  implicit class EitherExt2[A, B](ei: Either[A, B]) {

    def explicitGet(): B = ei match {
      case Left(value)  => throw new Exception(value.toString)
      case Right(value) => value
    }

    def foldToTry: Try[B] = {
      ei.fold(
        left => Failure(new Exception(left.toString)),
        right => Success(right)
      )
    }
  }
}
