package com.wavesplatform.lang.v1.estimator.v3

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.task.TaskM
import shapeless.{Lens, lens}

case class FunctionInfo(
    cost: Long,
    usedRefs: Set[String],
    nativeCost: Long
)

private[v3] case class EstimatorContext(
    funcs: Map[FunctionHeader, FunctionInfo],
    usedRefs: Set[String] = Set.empty
)

private[v3] object EstimatorContext {
  type EvalM[A] = TaskM[EstimatorContext, ExecutionError, A]

  object Lenses {
    val funcs: Lens[EstimatorContext, Map[FunctionHeader, FunctionInfo]] = lens[EstimatorContext] >> Symbol("funcs")
    val usedRefs: Lens[EstimatorContext, Set[String]]                    = lens[EstimatorContext] >> Symbol("usedRefs")
  }
}
