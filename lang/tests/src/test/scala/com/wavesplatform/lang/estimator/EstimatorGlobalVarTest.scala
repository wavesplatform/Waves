package com.wavesplatform.lang.estimator
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

class EstimatorGlobalVarTest extends ScriptEstimatorTestBase(ScriptEstimatorV3(fixOverflow = true, overhead = false)) {
  property("global variable cost should not overlap function argument cost") {
    estimate(
      """
        | func f(a: Boolean) = a
        | let a = groth16Verify(base58'', base58'', base58'')
        | f(true)
      """.stripMargin
    ) shouldBe Right(1)
  }
}
