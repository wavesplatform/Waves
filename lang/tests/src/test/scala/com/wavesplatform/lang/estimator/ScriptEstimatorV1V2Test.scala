package com.wavesplatform.lang.estimator

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.V3
import com.wavesplatform.lang.utils.functionCosts
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.ScriptEstimatorV1
import com.wavesplatform.lang.v1.estimator.v2.ScriptEstimatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.sumLong
import monix.eval.Coeval

class ScriptEstimatorV1V2Test extends ScriptEstimatorTestBase(ScriptEstimatorV1, ScriptEstimatorV2) {
  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(Plus, List(CONST_LONG(1), acc))
    }
    estimate(customFunctionCosts, expr).explicitGet() shouldBe 1 + (100 + 1) * 100000
  }

  property("handles const expression correctly") {
    estimate(customFunctionCosts, compile("false")).explicitGet() shouldBe 1
  }

  property("handles getter expression correctly") {
    estimate(customFunctionCosts, compile("tx.amount")).explicitGet() shouldBe 2 + 2
  }

  property("evaluates let statement lazily") {
    val eager = "let t = 1+1; t"
    estimate(customFunctionCosts, compile(eager)).explicitGet() shouldBe 5 + 102 + 2

    val lzy = "let t = 1+1; 2" // `t` is unused
    estimate(customFunctionCosts, compile(lzy)).explicitGet() shouldBe 5 + 1

    val onceOnly = "let x = 2+2; let y = x-x; x-y" // evaluated once only
    estimate(customFunctionCosts, compile(onceOnly)).explicitGet() shouldBe (5 + 102) + (5 + 14) + 14
  }

  property("ignores unused let statements") {
    val script = "let a = 1+2; let b = 2; let c = a+b; b" // `a` and `c` are unused
    estimate(customFunctionCosts, compile(script)).explicitGet() shouldBe 5 + (5 + 1) + 5 + 2
  }

  property("recursive let statement") {
    // let v = v; v
    val expr = BLOCK(LET("v", REF("v")), REF("v"))
    estimate(customFunctionCosts, expr).explicitGet() shouldBe 5 + 2 + 2
  }

  property("evaluates if statement lazily") {
    val script = "let a = 1+2; let b = 3+4; let c = if (tx.amount > 5) then a else b; c"
    estimate(customFunctionCosts, compile(script)).explicitGet() shouldBe (5 + 102) + 5 + (5 + 16 + 2) + 2
  }

  property("evaluates simple expression - const") {
    val expr = CONST_LONG(42)
    estimate(customFunctionCosts, expr).explicitGet() shouldBe 1
  }

  property("evaluates simple expression - let + const + ref") {
    val expr = BLOCK(
      LET("x", CONST_LONG(42)),
      REF("x")
    )
    estimate(customFunctionCosts, expr).explicitGet() shouldBe 8
  }

  property("recursive let block") {
    val expr = BLOCK(
      LET("x", REF("y")),
      BLOCK(LET("y", REF("x")), IF(TRUE, REF("x"), REF("y")))
    )
    estimate(customFunctionCosts, expr).explicitGet() shouldBe 5 + 5 + 2 + 2 + 2 + 2
  }

  property("evaluates simple expression - let + func_call + ref") {
    val functionCosts: Map[FunctionHeader, Coeval[Long]] = Map(Plus -> Coeval.now(1L))

    val expr = BLOCK(
      LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(1), CONST_LONG(2)))),
      REF("x")
    )
    estimate(functionCosts, expr).explicitGet() shouldBe 5 + 1 + 1 + 1 + 2
  }

  property("estimate script with func statement") {
    val exprWithoutFuncCall = BLOCK(
      Terms.FUNC(
        "first",
        List("arg1", "arg2"),
        LET_BLOCK(
          LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(3), CONST_LONG(1)))),
          REF("x")
        )
      ),
      BLOCK(
        LET("y", CONST_LONG(5)),
        REF("y")
      )
    )

    val exprWithFuncCall = BLOCK(
      Terms.FUNC(
        "first",
        List("arg1"),
        REF("arg1")
      ),
      FUNCTION_CALL(FunctionHeader.User("first"), List(CONST_LONG(1)))
    )

    estimate(customFunctionCosts, exprWithoutFuncCall) shouldBe Right(5 + 5 + 1 + 2)
    estimate(customFunctionCosts, exprWithFuncCall) shouldBe Right(5 + 5 + 2 + 1 + 1)
  }

  property("backward func invoke") {
    val script =
      """
        | func f(a: Int) = 1
        | func g(a: Int) = 2
        |
        | f(g(1))
        |
        |""".stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(23)
  }

  property("overlapped func") {
    val script =
      """
        | func f() = {
        |   func f() = {
        |     func f() = {
        |       1
        |     }
        |     f()
        |   }
        |   f()
        | }
        | f()
        |
        |""".stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(16)
  }

  property("multiple func calls") {
    val script =
      """
        | func f(a: Int) = a + 1
        | func g(a: Int) = f(1) + f(a)
        | func h(a: Int) = f(1) + g(a) + g(a)
        |
        | let a = 1
        | let b = 1
        | if (h(a) == f(a) + g(a))
        |   then h(a) + f(a) + g(a)
        |   else h(b) + f(b) + g(b)
        |
        |""".stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(290)
  }

  property("type constructor") {
    val script =
      """
        | let a = Address(base58'')
        | let b = Unit()
        | if (true) then a else b
      """.stripMargin

    estimate(functionCosts(V3), compile(script)) shouldBe Right(16)
  }

  property("function call with differing param referencing") {
    val script = s"""
                    | func inc(a: Int, b: Int, c: Int) = a + a + b
                    | inc(1 + 1, 1, 1)
                  """.stripMargin

    val expr = compile(script)
    val cost = estimate(functionCosts(V3), expr).explicitGet()

    cost shouldBe
        5     /* func decl              */ +
        5 * 3 /* func call args count   */ +
        1     /* first call a at inc()  */ +
        2     /* call a at inc()        */ +
        1     /* call + at inc()        */ +
        2     /* call a at inc()        */ +
        1     /* call + at inc()        */ +
        1     /* first call b at inc()  */ +
        2     /* call b at inc()        */ +
        3     /* 1 + 1 param            */ +
        1     /* 1 param                */ +
        1     /* 1 param                */
  }

}
