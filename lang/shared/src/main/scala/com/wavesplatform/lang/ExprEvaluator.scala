package com.wavesplatform.lang

trait ExprEvaluator extends Versioned {
  def apply[T](ctx: version.CtxT, expr: version.ExprT): Either[ExecutionError, T]
}
object ExprEvaluator {
  type LetExecResult  = Either[ExecutionError, Any]
  type LogItem        = (String, LetExecResult)
  type Log            = List[LogItem]
  type LogCallback    = LetExecResult => Unit
  type LetLogCallback = String => LogCallback
}
