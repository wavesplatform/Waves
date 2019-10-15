package com.wavesplatform.utils

import cats.Id
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.compiler.Terms.{FUNCTION_CALL, TRUE}
import com.wavesplatform.lang.v1.compiler.Types.BOOLEAN
import com.wavesplatform.lang.v1.evaluator.ctx.{EvaluationContext, UserFunction}
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.state.diffs.smart.predef.chainId
import com.wavesplatform.transaction.smart.WavesEnvironment
import monix.eval.Coeval
import org.scalatest.{FreeSpec, Matchers}

class UtilsSpecification extends FreeSpec with Matchers {
  private val environment = new WavesEnvironment(chainId, Coeval(???), null, EmptyBlockchain, Coeval(null))

  "estimate()" - {
    "handles functions that depend on each other" in {
      val callee = UserFunction[Environment]("callee", 0, BOOLEAN)(TRUE)
      val caller = UserFunction[Environment]("caller", 0, BOOLEAN)(FUNCTION_CALL(callee.header, List.empty))
      val ctx = EvaluationContext.build[Id, Environment](
        environment,
        typeDefs = Map.empty,
        letDefs = Map.empty,
        functions = Seq(caller, callee)
      )
      estimate(V3, ctx).size shouldBe 2
    }
  }
}
