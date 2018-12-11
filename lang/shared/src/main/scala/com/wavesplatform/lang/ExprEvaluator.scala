package com.wavesplatform.lang
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED

trait ExprEvaluator extends Versioned {
  def apply[A <: EVALUATED](ctx: version.CtxT, expr: version.ExprT): Either[ExecutionError, A]
}
object ExprEvaluator {
  type LetExecResult  = Either[ExecutionError, Any]
  type LogItem        = (String, LetExecResult)
  type Log            = List[LogItem]
  type LogCallback    = LetExecResult => Unit
  type LetLogCallback = String => LogCallback
}
