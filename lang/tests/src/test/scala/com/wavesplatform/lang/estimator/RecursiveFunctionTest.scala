package com.wavesplatform.lang.estimator

import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.{BLOCK, FUNC, FUNCTION_CALL}
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

class RecursiveFunctionTest extends ScriptEstimatorTestBase(ScriptEstimatorV1, ScriptEstimatorV2, ScriptEstimatorV3) {
  property("recursive func block") {
    val expr = BLOCK(
      FUNC("x", List.empty, FUNCTION_CALL(FunctionHeader.User("y"), List.empty)),
      BLOCK(FUNC("y", List.empty, FUNCTION_CALL(FunctionHeader.User("x"), List.empty)), FUNCTION_CALL(FunctionHeader.User("y"), List.empty))
    )
    estimate(customFunctionCosts, expr) shouldBe Symbol("left")
  }

  property("overlapped func with recursion") {
    val expr =
      BLOCK(
        FUNC(
          "f",
          Nil,
          BLOCK(
            FUNC("g", Nil, FUNCTION_CALL(User("f"), Nil)),
            BLOCK(
              FUNC(
                "f",
                Nil,
                BLOCK(
                  FUNC("f", Nil, FUNCTION_CALL(User("g"), Nil)),
                  FUNCTION_CALL(User("f"), Nil)
                )
              ),
              FUNCTION_CALL(User("f"), Nil)
            )
          )
        ),
        FUNCTION_CALL(User("f"), Nil)
      )

    estimate(functionCosts(V3), expr) shouldBe Symbol("left")
  }
}
