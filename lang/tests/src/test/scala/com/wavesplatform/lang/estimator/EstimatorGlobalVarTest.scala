package com.wavesplatform.lang.estimator
import com.wavesplatform.lang.directives.values.{Expression, V6}
import com.wavesplatform.lang.utils
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Decompiler
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.v3.ScriptEstimatorV3
import com.wavesplatform.lang.v1.evaluator.FunctionIds.BLS12_GROTH16_VERIFY

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

  property("avoid excessive overhead for single reference in function body with let overlap") {
    val expr =
      BLOCK(
        LET("a", FUNCTION_CALL(Native(BLS12_GROTH16_VERIFY), Nil)),
        BLOCK(
          FUNC(
            "g",
            Nil,
            BLOCK(
              LET("a", CONST_LONG(1)),
              CONST_BOOLEAN(true)
            )
          ),
          BLOCK(
            FUNC("f", Nil, REF("a")),
            IF(FUNCTION_CALL(User("g"), Nil), FUNCTION_CALL(User("f"), Nil), FUNCTION_CALL(User("f"), Nil))
          )
        )
      )
    Decompiler(expr, utils.getDecompilerContext(V6, Expression)).replaceAll("\\s", "") shouldBe
      """
        | let a = groth16Verify()
        | func g() = {
        |   let a = 1
        |   true
        | }
        | func f() = a
        | if (g()) then f() else f()
      """.stripMargin.replaceAll("\\s", "")
    estimate(functionCosts(V6), expr) shouldBe Right(2702)
    estimateFixed(expr) shouldBe Right(2701)
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
