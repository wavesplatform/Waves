package com.wavesplatform

import com.wavesplatform.state2.reader.SnapshotStateReader
import monix.eval.Coeval
import scorex.transaction.ValidationError.GenericError
import scorex.transaction.{Transaction, ValidationError}

import scala.util.{Left, Right, Try}

package object state2 {

  type StateReader = Coeval[SnapshotStateReader]

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
}
