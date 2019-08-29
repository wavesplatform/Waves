package com.wavesplatform.lang.v2

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.v1.ScriptEstimatorTest
import com.wavesplatform.lang.utils._
import com.wavesplatform.lang.v2.estimator.ScriptEstimatorV2

class ScriptEstimatorV2Test extends ScriptEstimatorTest(ScriptEstimatorV2) {
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
}
