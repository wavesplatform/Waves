package com.wavesplatform.lang.v1.estimator.v3

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.estimator.EstimationError
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.task.TaskM
import monix.eval.Coeval

private[v3] case class EstimatorContext(
    funcs: Map[FunctionHeader, (Coeval[Long], Set[String])],
    usedRefs: Set[String] = Set(),
    refsCosts: Map[String, EvalM[Long]] = Map(),
    globalFunctionsCosts: Map[String, Long] = Map(), //
    globalLetsCosts: Map[String, Long] = Map(),      // only for globalDeclarationsMode
    globalLetEvals: Map[String, EvalM[Long]] = Map() //
)

private[v3] object EstimatorContext {
  type EvalM[A] = TaskM[EstimatorContext, EstimationError, A]
}
