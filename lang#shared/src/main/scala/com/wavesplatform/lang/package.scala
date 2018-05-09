package com.wavesplatform

import cats.data.EitherT
import monix.eval.Coeval

package object lang {

  type ExecutionError           = String
  type ExecutionLog             = String
  type TrampolinedExecResult[T] = EitherT[Coeval, ExecutionError, T]

}
