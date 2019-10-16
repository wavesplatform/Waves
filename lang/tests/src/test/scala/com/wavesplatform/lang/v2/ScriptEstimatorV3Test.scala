package com.wavesplatform.lang.v2

import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.ScriptEstimatorTestBase
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV3

class ScriptEstimatorV3Test extends ScriptEstimatorTestBase(ScriptEstimatorV1, ScriptEstimatorV3) {
  property("isolated if/then blocks estimation") {
    def expr(invokeValInThenBlock: Boolean): String =
      s"""
         | func complex() = 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1 + 1
         |
         | let a = complex()
         | let b = a + complex()
         | if (true)
         |   then WriteSet([${ if (invokeValInThenBlock) """DataEntry("a", a)""" else "" }])
         |   else WriteSet([DataEntry("a", b)])
      """.stripMargin

    val costs = functionCosts(V3)
    estimate(costs, compile(expr(true))) shouldBe estimate(costs, compile(expr(false)))
  }

  property("let block outer scope reach") {
    val script =
      """
        | let a = 1
        | let b = a
        | let c = {
        |   let unused = 1
        |   a
        | }
        | c == b
      """.stripMargin

    estimate(functionCosts(V3), compile(script))  shouldBe Right(30)
  }
}
