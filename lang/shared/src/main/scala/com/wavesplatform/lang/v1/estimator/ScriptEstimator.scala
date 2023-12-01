package com.wavesplatform.lang.v1.estimator

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import monix.eval.Coeval

trait ScriptEstimator {
  val version: Int

  def apply(
    declaredVals:  Set[String],
    functionCosts: Map[FunctionHeader, Coeval[Long]],
    expr:          EXPR
  ): Either[String, Long]
}

object ScriptEstimator {
  def all(fixOverflow: Boolean): List[ScriptEstimator] =
    List(ScriptEstimatorV1, ScriptEstimatorV2, ScriptEstimatorV3(fixOverflow, overhead = false, letFixes = true))
}
