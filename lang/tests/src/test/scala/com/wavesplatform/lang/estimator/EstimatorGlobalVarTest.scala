package com.wavesplatform.lang.estimator
import com.wavesplatform.lang.directives.values.V6
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3

class EstimatorGlobalVarTest extends ScriptEstimatorTestBase(ScriptEstimatorV3(fixOverflow = true, overhead = false, letFixes = false)) {
  private val fixedEstimator                                      = ScriptEstimatorV3(fixOverflow = true, overhead = false, letFixes = true)
  private def estimateFixed(expr: EXPR): Either[String, Long]     = fixedEstimator(lets, functionCosts(V6), expr)
  private def estimateFixed(script: String): Either[String, Long] = estimateFixed(compile(script)(V6))

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

  property("avoid excessive overhead for single reference in function body") {
    val script =
      """
        | let a = groth16Verify(base58'', base58'', base58'')
        | func f() = a
        | f()
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(2700)
  }

  property("avoid excessive overhead for expression in function argument") {
    val script =
      """
        | func f(a: Int) = a
        | f(1 + 2 + 3)
      """.stripMargin
    estimate(script) shouldBe Right(3)
    estimateFixed(script) shouldBe Right(2)
  }

  property("avoid excessive overhead for function argument referencing to global variable") {
    val script =
      """
        | let a = groth16Verify(base58'', base58'', base58'')
        | func f(a: Boolean) = a
        | f(a)
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(2700)
  }

  property("correctly count overhead for blank reference in function body") {
    val script =
      """
        | let a = 1
        | func f() = a
        | f()
      """.stripMargin
    estimate(script) shouldBe Right(1)
    estimateFixed(script) shouldBe Right(1)
  }

  property("mixed case") {
    val script =
      """
        | func f(a: Boolean) = a
        | let a = groth16Verify(base58'', base58'', base58'')
        | func g() = f(a)
        | g()
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(2700)
  }

  property("mixed case 2") {
    val script =
      """
        | func f(a: Boolean) = a
        | let a = groth16Verify(base58'', base58'', base58'')
        | func g() = f(true)
        | g()
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(1)
  }
}
