package com.wavesplatform

import cats.data.EitherT
import monix.eval.Coeval

import scala.util.{Left, Right}

package object lang {
  type ExecutionError           = String
  type ExecutionLog             = String
  type TrampolinedExecResult[T] = EitherT[Coeval, ExecutionError, T]

  implicit class EitherExt3[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value)  => throw new Exception(s"$value")
      case Right(value) => value
    }
  }

}
