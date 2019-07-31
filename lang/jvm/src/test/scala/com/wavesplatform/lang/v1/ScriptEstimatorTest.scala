package com.wavesplatform.lang.v1

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Global
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.script.Script
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler, Terms}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext.sumLong
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptEstimatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  val Plus  = FunctionHeader.Native(SUM_LONG)
  val Minus = FunctionHeader.Native(SUB_LONG)
  val Gt    = FunctionHeader.Native(GT_LONG)

  val FunctionCosts: Map[FunctionHeader, Coeval[Long]] = Map[FunctionHeader, Long](Plus -> 100, Minus -> 10, Gt -> 10).mapValues(Coeval.now)

  private val ctx = {
    val transactionType = Types.buildTransferTransactionType(true)
    val tx              = CaseObj(transactionType, Map("amount" -> CONST_LONG(100000000L)))
    Monoid
      .combine(
        PureContext.build(Global, V1),
        CTX(
          Seq(transactionType),
          Map(("tx", (transactionType, LazyVal(EitherT.pure(tx))))),
          Array.empty
        )
      )
  }

  private def compile(code: String): EXPR = {
    val untyped = Parser.parseExpr(code).get.value
    ExpressionCompiler(ctx.compilerContext, untyped).map(_._1).explicitGet()
  }

  private def estimate(functionCosts: collection.Map[FunctionHeader, Coeval[Long]], script: EXPR) =
    ScriptEstimator(ctx.evaluationContext.letDefs.keySet, functionCosts, script)

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(Plus, List(CONST_LONG(1), acc))
    }
    estimate(FunctionCosts, expr) shouldBe 'right
  }

  property("handles const expression correctly") {
    estimate(Map.empty, compile("false")).explicitGet() shouldBe 1
  }

  property("handles getter expression correctly") {
    estimate(Map.empty, compile("tx.amount")).explicitGet() shouldBe 2 + 2
  }

  property("evaluates let statement lazily") {
    val eager = "let t = 1+1; t"
    estimate(FunctionCosts, compile(eager)).explicitGet() shouldBe 5 + 102 + 2

    val lzy = "let t = 1+1; 2" // `t` is unused
    estimate(FunctionCosts, compile(lzy)).explicitGet() shouldBe 5 + 1

    val onceOnly = "let x = 2+2; let y = x-x; x-y" // evaluated once only
    estimate(FunctionCosts, compile(onceOnly)).explicitGet() shouldBe (5 + 102) + (5 + 14) + 14
  }

  property("ignores unused let statements") {
    val script = "let a = 1+2; let b = 2; let c = a+b; b" // `a` and `c` are unused
    estimate(FunctionCosts, compile(script)).explicitGet() shouldBe 5 + (5 + 1) + 5 + 2
  }

  property("recursive let statement") {
    // let v = v; v
    val expr = BLOCK(LET("v", REF("v")), REF("v"))
    estimate(Map.empty, expr) shouldBe 'right
  }

  property("evaluates if statement lazily") {
    val script = "let a = 1+2; let b = 3+4; let c = if (tx.amount > 5) then a else b; c"
    estimate(FunctionCosts, compile(script)).explicitGet() shouldBe (5 + 102) + 5 + (5 + 16 + 2) + 2
  }

  property("evaluates simple expression - const") {
    val expr = CONST_LONG(42)
    estimate(FunctionCosts, expr).explicitGet() shouldBe 1
  }

  property("evaluates simple expression - let + const + ref") {
    val expr = BLOCK(
      LET("x", CONST_LONG(42)),
      REF("x")
    )
    estimate(FunctionCosts, expr).explicitGet() shouldBe 8
  }

  property("recursive let block") {
    val expr = BLOCK(
      LET("x", REF("y")),
      BLOCK(LET("y", REF("x")), IF(TRUE, REF("x"), REF("y")))
    )
    estimate(FunctionCosts, expr).explicitGet() shouldBe 18
  }

  property("recursive func block") {
    val expr = BLOCK(
      FUNC("x", List.empty, FUNCTION_CALL(FunctionHeader.User("y"), List.empty)),
      BLOCK(FUNC("y", List.empty, FUNCTION_CALL(FunctionHeader.User("x"), List.empty)), FUNCTION_CALL(FunctionHeader.User("y"), List.empty))
    )
    estimate(FunctionCosts, expr) shouldBe 'left
  }

  property("evaluates simple expression - let + func_call + ref") {
    val functionCosts: Map[FunctionHeader, Coeval[Long]] = Map[FunctionHeader, Long](Plus -> 1).mapValues(Coeval.now)

    val expr = BLOCK(
      LET("x", FUNCTION_CALL(sumLong.header, List(CONST_LONG(1), CONST_LONG(2)))),
      REF("x")
    )
    estimate(functionCosts, expr).explicitGet() shouldBe 10
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

    estimate(FunctionCosts, exprWithoutFuncCall) shouldBe Right(5 + 5 + 1 + 2)
    estimate(FunctionCosts, exprWithFuncCall) shouldBe Right(5 + 5 + 2 + 1 + 1)
  }

  property("script complexity should be constant") {
    val script = "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"
    Script.fromBase64String(script).explicitGet().complexity shouldBe 1970
  }
}
