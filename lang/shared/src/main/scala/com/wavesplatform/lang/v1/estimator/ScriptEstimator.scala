package com.wavesplatform.lang.v1.estimator

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import monix.eval.Coeval

trait ScriptEstimator {
  def apply(
    declaredVals:  Set[String],
    functionCosts: Map[FunctionHeader, Coeval[Long]],
    expr:          EXPR
  ): Either[String, Long]
}
