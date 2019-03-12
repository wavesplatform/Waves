package com.wavesplatform.utils

import com.wavesplatform.lang.StdLibVersion
import com.wavesplatform.lang.v1.compiler.Terms.{FUNCTION_CALL, TRUE}
import com.wavesplatform.lang.v1.compiler.Types.BOOLEAN
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, UserFunction}
import org.scalatest.{FreeSpec, Matchers}

class UtilsSpecification extends FreeSpec with Matchers {

  "estimate()" - {
    "handles functions that depend on each other" in {
      val callee = UserFunction("callee", 0, BOOLEAN, "test users true")(TRUE)
      val caller = UserFunction("caller", 0, BOOLEAN, "test call")(FUNCTION_CALL(callee.header, List.empty))
      val ctx = EvaluationContext(
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Seq(caller, callee).map(f => f.header -> f)(collection.breakOut)
      )
      estimate(StdLibVersion.V3, ctx).size shouldBe 2
    }
  }
}
