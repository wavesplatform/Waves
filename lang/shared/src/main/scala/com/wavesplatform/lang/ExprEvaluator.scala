package com.wavesplatform.lang

trait ExprEvaluator extends Versioned {
  def apply[T](ctx: version.CtxT, expr: version.ExprT): (version.CtxT, Either[ExecutionError, T])
}
