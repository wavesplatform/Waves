package com.wavesplatform

import cats.Eval
import cats.data.EitherT

package object lang {

  type ExecutionError           = String
  type ExecutionLog             = String
  type TrampolinedExecResult[F[_], T] = EitherT[λ[q => Eval[F[q]]], ExecutionError, T]

}
