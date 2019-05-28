package com.wavesplatform

import cats.Eval
import cats.data.EitherT

package object lang {

  type ExecutionError           = String
  type ExecutionLog             = String
  type TrampolinedExecResult[T] = EitherT[Eval, ExecutionError, T]

}
