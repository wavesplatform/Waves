package com.wavesplatform

import cats.Eval
import cats.data.EitherT

package object lang {
  implicit def toError(msg: String): CommonError = CommonError(msg)

  type ExecutionLog   = String

  type EvalF[F[_], A]                 = Eval[F[A]]
  type TrampolinedExecResult[F[_], T] = EitherT[EvalF[F, *], ExecutionError, T]
}
