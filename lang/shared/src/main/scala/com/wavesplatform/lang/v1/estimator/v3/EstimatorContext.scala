package com.wavesplatform.lang.v1.estimator.v3


import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.estimator.EstimationError
import com.wavesplatform.lang.v1.task.TaskM
import monix.eval.Coeval
import shapeless.{Lens, lens}

private[v3] case class EstimatorContext(
    funcs: Map[FunctionHeader, (Coeval[Long], Set[String])],
    usedRefs: Set[String] = Set.empty
)

private[v3] object EstimatorContext {
  type EvalM[A] = TaskM[EstimatorContext, EstimationError, A]

  object Lenses {
    val funcs: Lens[EstimatorContext, Map[FunctionHeader, (Coeval[Long], Set[String])]] = lens[EstimatorContext] >> Symbol("funcs")
    val usedRefs: Lens[EstimatorContext, Set[String]]                                   = lens[EstimatorContext] >> Symbol("usedRefs")
  }
}
