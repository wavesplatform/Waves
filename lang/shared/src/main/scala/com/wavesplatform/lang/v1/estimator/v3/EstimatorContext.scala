package com.wavesplatform.lang.v1.estimator.v3

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.task.TaskM
import shapeless.{Lens, lens}

private[v3] case class EstimatorContext(
  funcs:    Map[FunctionHeader, Long],
  usedRefs: Set[String] = Set.empty
)

private[v3] object EstimatorContext {
  type EvalM[A] = TaskM[EstimatorContext, ExecutionError, A]

  object Lenses {
    val funcs: Lens[EstimatorContext, Map[FunctionHeader, Long]]  = lens[EstimatorContext] >> 'funcs
    val usedRefs: Lens[EstimatorContext, Set[String]]             = lens[EstimatorContext] >> 'usedRefs
  }
}
