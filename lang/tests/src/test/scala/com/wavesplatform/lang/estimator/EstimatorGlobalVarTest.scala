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
        | func f(a: Boolean) = {
        |   func g(b1: Boolean, b2: Boolean) = b1 || b2
        |   a || g(true, true)
        | }
        | let a  = groth16Verify(base58'', base58'', base58'')
        | let b1 = groth16Verify(base58'', base58'', base58'')
        | let b2 = groth16Verify(base58'', base58'', base58'')
        | f(true)
      """.stripMargin
    estimate(script) shouldBe Right(8101)
    estimateFixed(script) shouldBe Right(1)
  }

  property("avoid redundant overhead for single reference in function body") {
    val script =
      """
        | let a = groth16Verify(base58'', base58'', base58'')
        | func f() = a
        | f()
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(2700)
  }

  property("leave unchanged adding overhead complexity for function with zero complexity body") {
    val script =
      """
        | let a = groth16Verify(base58'', base58'', base58'')
        | func f(a: Boolean) = a
        | f(a)
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(2701)
  }

  property("avoid redundant overhead for single reference in function body with let overlap") {
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

  property("direct call blank function f() adds 1 complexity") {
    val script =
      """
        | let a = groth16Verify(base58'', base58'', base58'')
        | func f(x: Boolean) = x
        | func g(y: Boolean) = f(y)
        | func h(z: Boolean) = g(z)
        | h(a)
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(2701)
  }

  property("call chain with unused global variable") {
    val script =
      """
        | let a = groth16Verify(base58'', base58'', base58'')
        | func f(a: Boolean) = a
        | func g(a: Boolean) = f(a)
        | func h()           = g(true)
        | h()
      """.stripMargin
    estimate(script) shouldBe Right(2701)
    estimateFixed(script) shouldBe Right(1)
  }

  property("overhead for expression in blank function argument (as evaluator)") {
    val script =
      """
        | func f(a: Int) = a
        | f(1 + 2 + 3)
      """.stripMargin
    estimate(script) shouldBe Right(3)
    estimateFixed(script) shouldBe Right(3)
  }
}
