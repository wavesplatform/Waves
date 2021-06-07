package com.wavesplatform.lang.v1.estimator.v3

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.evaluator.FunctionIds.FOLD
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.task.TaskM
import monix.eval.Coeval
import shapeless.{Lens, lens}

private[v3] case class EstimatorContext(
    funcs: Map[FunctionHeader, (Coeval[Long], Set[String])],
    usedRefs: Set[String] = Set.empty
)

private[v3] object EstimatorContext {
  type EvalM[A] = TaskM[EstimatorContext, ExecutionError, A]

  object Lenses {
    val funcs: Lens[EstimatorContext, Map[FunctionHeader, (Coeval[Long], Set[String])]] = lens[EstimatorContext] >> Symbol("funcs")
    val usedRefs: Lens[EstimatorContext, Set[String]]                                   = lens[EstimatorContext] >> Symbol("usedRefs")
  }

  case class HighOrderFunctionInfo(callLimit: Int, functionIndex: Int)

  val highOrderFunctions: Map[FunctionHeader, HighOrderFunctionInfo] =
    PureContext.folds
      .map { case (limit, f) => (f.header, HighOrderFunctionInfo(limit, 2)) }
      .toMap
}
