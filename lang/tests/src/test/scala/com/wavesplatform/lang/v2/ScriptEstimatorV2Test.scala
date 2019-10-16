package com.wavesplatform.lang.v2

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v1.ScriptEstimatorTestBase
import com.wavesplatform.lang.v2.estimator.{ScriptEstimatorV2, ScriptEstimatorV3}

class ScriptEstimatorV2Test extends ScriptEstimatorTestBase(ScriptEstimatorV2, ScriptEstimatorV3) {
  property("transitive ref usage") {
    def estimateRefUsage(ref: String): Long = {
      val script = s"""
                      |  let me = addressFromStringValue("")
                      |  func get() = getStringValue($ref, "")
                      |  get() + get() + get() + get() + get()
                      |
                    """.stripMargin

      val expr = compile(script)
      estimate(functionCosts(V3), expr).explicitGet()
    }

    val addressFromStrComplexity = 124
    estimateRefUsage("me") - estimateRefUsage("this") shouldBe addressFromStrComplexity + 1
  }

  property("transitive ref usage with overlapping") {
    val firstCallCount = 6

    val script =
      s"""
         |  let me = 1 + 1 + 1 + 1
         |  func third(p: Int) = me
         |  func second(me: Int) = third(me)
         |  func first() = second(1)
         |  ${List.fill(firstCallCount)("first()").mkString(" + ")}
         |
       """.stripMargin

    val expr = compile(script)

    val firstCallCost =
      5 /* call second()                     */ +
      1 /* eval 1                            */ +
      3 /* eval param me and pass to third() */ +
      5 /* call third()                      */ +
      2 /* eval ref me                       */

    estimate(functionCosts(V3), expr).explicitGet() shouldBe
      firstCallCost * firstCallCount      +
      firstCallCount - 1 /* pluses */     +
      5 * 4              /* all blocks */ +
      7                  /* calc 1 + 1 + 1 + 1 exactly once */
  }
}
