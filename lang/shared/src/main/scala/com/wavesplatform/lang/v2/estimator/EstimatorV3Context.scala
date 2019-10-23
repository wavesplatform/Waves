package com.wavesplatform.lang.v2.estimator

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.task.TaskM
import shapeless.{Lens, lens}

case class EstimatorV3Context(
  funcs:    Map[FunctionHeader, Long],
  usedRefs: Set[String] = Set.empty
)

object EstimatorV3Context {
  type EvalM3[A] = TaskM[EstimatorV3Context, ExecutionError, A]

  object Lenses {
    val funcs: Lens[EstimatorV3Context, Map[FunctionHeader, Long]]  = lens[EstimatorV3Context] >> 'funcs
    val usedRefs: Lens[EstimatorV3Context, Set[String]]             = lens[EstimatorV3Context] >> 'usedRefs
  }
}
