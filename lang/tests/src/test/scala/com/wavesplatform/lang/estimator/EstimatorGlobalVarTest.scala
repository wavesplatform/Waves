package com.wavesplatform.lang.estimator
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

class EstimatorGlobalVarTest extends ScriptEstimatorTestBase(ScriptEstimatorV3(fixOverflow = true, overhead = false, letFixes = false)) {
  private def estimateFixed(script: String): Either[String, Long] =
    ScriptEstimatorV3(fixOverflow = true, overhead = false, letFixes = true)(lets, functionCosts(V6), compile(script)(V6))

  property("global variable cost should not overlap function argument cost") {
    val script =
      """
        | func f(a: Boolean) = a
        | let a = groth16Verify(base58'', base58'', base58'')
        | f(true)
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(1)
  }
}
