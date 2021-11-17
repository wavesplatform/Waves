package com.wavesplatform.lang.v1.estimator

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext

case class HighOrderFunctionInfo(callLimit: Int, functionIndex: Int)

object HighOrderFunctionInfo {
  val all: Map[FunctionHeader, HighOrderFunctionInfo] =
    PureContext.folds.map { case (limit, f) => (f.header, HighOrderFunctionInfo(limit, 2)) }.toMap
}
