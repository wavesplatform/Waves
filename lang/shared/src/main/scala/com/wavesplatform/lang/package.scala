package com.wavesplatform

import cats.data.EitherT
import com.wavesplatform.lang.Terms.{FUNCTION, TYPE}
import monix.eval.Coeval

package object lang {

  type ExecutionError           = String
  type TrampolinedExecResult[T] = EitherT[Coeval, ExecutionError, T]
  type TypeDefs = Map[String, TYPE]
  type FunctionSigs = Map[FunctionHeader, FUNCTION]

}
