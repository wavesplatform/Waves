package com.wavesplatform.utils

import cats.Id
import com.wavesplatform.common.state.ByteStr
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils.*
import com.wavesplatform.lang.v1.compiler.Terms.{FUNCTION_CALL, TRUE}
import com.wavesplatform.lang.v1.compiler.Types.BOOLEAN
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, UserFunction}
import com.wavesplatform.test.FreeSpec
import com.wavesplatform.transaction.smart.WavesEnvironment

class UtilsSpecification extends FreeSpec {
  private val environment = WavesEnvironment(null, null, ByteStr.empty, DirectiveSet.contractDirectiveSet, EmptyBlockchain)

  "estimate()" - {
    "handles functions that depend on each other" in {
      val callee = UserFunction("callee", 0, BOOLEAN)(TRUE)
      val caller = UserFunction("caller", 0, BOOLEAN)(FUNCTION_CALL(callee.header, List.empty))
      val ctx = EvaluationContext[Id](
        environment,
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Map(caller.header -> caller, callee.header -> callee)
      )
      estimate(V3, ctx).size shouldBe 2
    }
  }
}
