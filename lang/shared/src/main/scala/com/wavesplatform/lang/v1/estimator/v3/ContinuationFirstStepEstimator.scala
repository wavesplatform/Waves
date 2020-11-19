package com.wavesplatform.lang.v1.estimator.v3

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.EXPR

object ContinuationFirstStepEstimator extends GenericScriptEstimatorV3(continuationFirstStepMode = true) {
  def estimate(
      funcs: Map[FunctionHeader, FunctionInfo],
      expr: EXPR
  ): Either[ExecutionError, Long] =
    evalExpr(expr, checkContinuationFirstStep = true)
      .run(EstimatorContext(funcs))
      .value
      ._2
}
