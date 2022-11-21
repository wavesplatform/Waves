package com.wavesplatform.lang.estimator

import com.wavesplatform.lang.v1.FunctionHeader.Native
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds.SUM_LONG

class CommonScriptEstimatorTest
    extends ScriptEstimatorTestBase(
      ScriptEstimatorV1,
      ScriptEstimatorV2,
      ScriptEstimatorV3(fixOverflow = true, overhead = true),
      ScriptEstimatorV3(fixOverflow = true, overhead = false),
      ScriptEstimatorV3(fixOverflow = false, overhead = true),
      ScriptEstimatorV3(fixOverflow = false, overhead = false),
      evaluatorV2AsEstimator(overhead = true),
      evaluatorV2AsEstimator(overhead = false)
    ) {

  property("context leak") {
    def script(ref: String) =
      compile {
        s"""
           |  func inc($ref: Int) = $ref + 1
           |  let xxx = 5
           |  inc(xxx)
         """.stripMargin
      }
    estimateDelta(script("xxx"), script("y")) shouldBe Right(0)
  }

  property("let overlapping") {
    def expr(outerLet: String): EXPR =
      BLOCK(
        LET(outerLet, FUNCTION_CALL(Native(SUM_LONG), List(CONST_LONG(1), CONST_LONG(1)))),
        BLOCK(
          LET(
            "b",
            BLOCK(
              LET("a", CONST_LONG(1)),
              CONST_LONG(1)
            )
          ),
          FUNCTION_CALL(Native(SUM_LONG), List(REF(outerLet), REF("b")))
        )
      )

    /*
      let a = 1 + 1
      let b = {
        let a = 1
        1
      }
      a + b
     */
    estimateDelta(expr("a"), expr("x")) shouldBe Right(0)
  }
}
