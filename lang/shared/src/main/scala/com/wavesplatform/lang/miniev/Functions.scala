package com.wavesplatform.lang.miniev

import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.evaluator.ctx.BaseFunction
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext

object Functions {
  val predefinedFunctions: Map[FunctionHeader, BaseFunction] = PureContext.v6Functions.map {
    bf => bf.header -> bf
  }.toMap
}
