package com.wavesplatform.lang.v1

import cats.data.EitherT
import cats.syntax.semigroup._
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.FunctionHeader.FunctionHeaderType
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.transferTransactionType
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}

class ScriptEstimatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  val Plus  = FunctionHeader("+", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
  val Minus = FunctionHeader("-", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))
  val Gt    = FunctionHeader(">", List(FunctionHeaderType.LONG, FunctionHeaderType.LONG))

  val FunctionCosts: Map[FunctionHeader, Long] = Map(Plus -> 100, Minus -> 10, Gt -> 10)

  private val ctx: CompilerContext = {
    // make up a `tx` object
    val tx = CaseObj(transferTransactionType.typeRef, Map("amount" -> Val(LONG)(100000000)))
    val txCtx = EvaluationContext(
      caseTypeDefs = Map(transferTransactionType.name -> transferTransactionType),
      letDefs = Map("tx"                              -> LazyVal(transferTransactionType.typeRef)(EitherT.pure(tx))),
      functions = Map.empty
    )
    CompilerContext.fromEvaluationContext(PureContext.instance |+| txCtx, Map.empty)
  }

  private def compile(code: String): EXPR = {
    val untyped = Parser(code).get.value
    require(untyped.size == 1)
    CompilerV1(ctx, untyped.head).right.get
  }

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(Plus, List(CONST_LONG(1), acc), LONG)
    }
    ScriptEstimator(FunctionCosts, expr) shouldBe 'right
  }

  property("handles const expression correctly") {
    ScriptEstimator(Map.empty, compile("false")).right.get shouldBe 1
  }

  property("handles getter expression correctly") {
    ScriptEstimator(Map.empty, compile("tx.amount")).right.get shouldBe 2 + 2
  }

  property("evaluates let statement lazily") {
    val eager = "let t = 1+1; t"
    ScriptEstimator(FunctionCosts, compile(eager)).right.get shouldBe 5 + 102 + 2

    val lzy = "let t = 1+1; 2" // `t` is unused
    ScriptEstimator(FunctionCosts, compile(lzy)).right.get shouldBe 5 + 1

    val onceOnly = "let x = 2+2; let y = x-x; x-y" // evaluated once only
    ScriptEstimator(FunctionCosts, compile(onceOnly)).right.get shouldBe (5 + 102) + (5 + 14) + 14
  }

  property("ignores unused let statements") {
    val script = "let a = 1+2; let b = 2; let c = a+b; b" // `a` and `c` are unused
    ScriptEstimator(FunctionCosts, compile(script)).right.get shouldBe 5 + (5 + 1) + 5 + 2
  }

  property("recursive let statement") {
    // let v = v; v
    val expr = BLOCK(LET("v", REF("v", LONG)), REF("v", LONG), LONG)
    ScriptEstimator(Map.empty, expr) shouldBe 'right
  }

  property("evaluates if statement lazily") {
    val script = "let a = 1+2; let b = 3+4; let c = if (tx.amount > 5) then a else b; c"
    ScriptEstimator(FunctionCosts, compile(script)).right.get shouldBe (5 + 102) + 5 + (5 + 16 + 2) + 2
  }
}
