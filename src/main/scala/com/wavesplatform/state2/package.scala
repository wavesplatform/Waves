package com.wavesplatform

import scorex.transaction.ValidationError.TransactionValidationError
import scorex.transaction.{StateValidationError, Transaction, ValidationError}

import scala.util.{Left, Right, Try}

package object state2 {

  type ByteArray = EqByteArray

  def safeSum(x: Long, y: Long): Long = Try(Math.addExact(x, y)).getOrElse(Long.MinValue)

  implicit class EitherExt[L <: ValidationError, R](ei: Either[L, R]) {
    def liftValidationError[T <: Transaction](t: T): Either[StateValidationError, R] = {
      ei.left.map(e => TransactionValidationError(t, e.toString))
    }
  }

  implicit class EitherExt2[A, B](ei: Either[A, B]) {
    def explicitGet(): B = ei match {
      case Left(value) => throw new Exception(value.toString)
      case Right(value) => value
    }
  }

}
