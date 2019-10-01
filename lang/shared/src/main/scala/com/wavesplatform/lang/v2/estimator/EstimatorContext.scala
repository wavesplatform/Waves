package com.wavesplatform.lang.v2.estimator

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.FUNC
import com.wavesplatform.lang.v1.task.TaskM
import com.wavesplatform.lang.v2.estimator.EstimatorContext.EvalM
import shapeless.{Lens, lens}

case class EstimatorContext(
  letDefs:     Map[String, (Boolean, EvalM[Long])],
  predefFuncs: Map[FunctionHeader, Long],
  userFuncs:   Map[FunctionHeader, FUNC] = Map.empty,
  overlappedRefs: Map[String, (Boolean, EvalM[Long])] = Map.empty
)

object EstimatorContext {
  type EvalM[A] = TaskM[EstimatorContext, ExecutionError, A]

  object Lenses {
    val lets: Lens[EstimatorContext, Map[String, (Boolean, EvalM[Long])]] = lens[EstimatorContext] >> 'letDefs
    val userFuncs: Lens[EstimatorContext, Map[FunctionHeader, FUNC]]      = lens[EstimatorContext] >> 'userFuncs
    val predefFuncs: Lens[EstimatorContext, Map[FunctionHeader, Long]]    = lens[EstimatorContext] >> 'predefFuncs
    val overlappedRefs: Lens[EstimatorContext, Map[String, (Boolean, EvalM[Long])]] = lens[EstimatorContext] >> 'overlappedRefs
  }
}
