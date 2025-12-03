package com.wavesplatform.lang.v1.estimator.v2

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNC
import com.wavesplatform.lang.v1.estimator.EstimationError
import com.wavesplatform.lang.v1.estimator.v2.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.task.TaskM

private[v2] case class EstimatorContext(
    letDefs: Map[String, (Boolean, EvalM[Long])],
    predefFuncs: Map[FunctionHeader, Long],
    userFuncs: Map[FunctionHeader, FUNC] = Map.empty,
    overlappedRefs: Map[String, (Boolean, EvalM[Long])] = Map.empty
)

private[v2] object EstimatorContext {
  type EvalM[A] = TaskM[EstimatorContext, EstimationError, A]
}
