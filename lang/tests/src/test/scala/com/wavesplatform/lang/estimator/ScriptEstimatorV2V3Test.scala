package com.wavesplatform.lang.estimator

import com.wavesplatform.lang.v1.compiler.Terms.EXPR
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

class ScriptEstimatorV2V3Test
    extends ScriptEstimatorTestBase(
      ScriptEstimatorV2,
      ScriptEstimatorV3(fixOverflow = true, overhead = true, letFixes = false),
      ScriptEstimatorV3(fixOverflow = false, overhead = true, letFixes = false),
      ScriptEstimatorV3(fixOverflow = true, overhead = true, letFixes = true)
    ) {
  property("transitive ref usage") {
    def refUsage(ref: String): EXPR =
      compile(
        s"""
           |  let me = addressFromStringValue("")
           |  func get() = getStringValue($ref, "")
           |  get() + get() + get() + get() + get()
           |
         """.stripMargin
      )

    val addressFromStrComplexity = 124
    estimateDelta(refUsage("this"), refUsage("me")) shouldBe Right(addressFromStrComplexity + 1)
  }

  property("overlapping should not change cost") {
    estimateDelta(scriptWithOverlapping("x", "1 + 1"), scriptWithOverlapping("me", "1 + 1")) shouldBe Right(0)
  }

  property("refs expr should be counted once") {
    estimateDelta(scriptWithOverlapping("me", "1"), scriptWithOverlapping("me", "1 + 1")) shouldBe Right(2)
  }

  private def scriptWithOverlapping(param: String, refExpr: String) = {
    val firstCallCount = 6
    val script =
      s"""
         |  let me = $refExpr
         |  func third(p: Int) = me
         |  func second($param: Int) = third($param)
         |  func first() = second(1)
         |  ${List.fill(firstCallCount)("first()").mkString(" + ")}
         |
       """.stripMargin

    compile(script)
  }
}
