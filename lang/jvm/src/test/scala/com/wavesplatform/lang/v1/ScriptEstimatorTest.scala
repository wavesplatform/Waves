package com.wavesplatform.lang.v1

import cats.data.EitherT
import cats.kernel.Monoid
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{CompilerContext, CompilerV1}
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.Types.transferTransactionType
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang._
import org.scalatest.prop.PropertyChecks
import org.scalatest.{Matchers, PropSpec}
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import monix.eval.Coeval

class ScriptEstimatorTest extends PropSpec with PropertyChecks with Matchers with ScriptGen with NoShrink {
  val Plus  = FunctionHeader.Native(SUM_LONG)
  val Minus = FunctionHeader.Native(SUB_LONG)
  val Gt    = FunctionHeader.Native(GT_LONG)

  val FunctionCosts: Map[FunctionHeader, Coeval[Long]] = Map[FunctionHeader, Long](Plus -> 100, Minus -> 10, Gt -> 10).mapValues(Coeval.now)

  private val ctx: CompilerContext = {
    val tx = CaseObj(transferTransactionType.typeRef, Map("amount" -> 100000000L))
    Monoid
      .combine(PureContext.ctx,
               CTX(
                 Seq(transferTransactionType),
                 Map(("tx", (transferTransactionType.typeRef, LazyVal(EitherT.pure(tx))))),
                 Seq.empty
               ))
      .compilerContext
  }

  private def compile(code: String): EXPR = {
    val untyped = Parser(code).get.value
    require(untyped.size == 1)
    CompilerV1(ctx, untyped.head).map(_._1).explicitGet()
  }

  property("successful on very deep expressions(stack overflow check)") {
    val expr = (1 to 100000).foldLeft[EXPR](CONST_LONG(0)) { (acc, _) =>
      FUNCTION_CALL(Plus, List(CONST_LONG(1), acc))
    }
    ScriptEstimator(FunctionCosts, expr) shouldBe 'right
  }

  property("handles const expression correctly") {
    ScriptEstimator(Map.empty, compile("false")).explicitGet() shouldBe 1
  }

  property("handles getter expression correctly") {
    ScriptEstimator(Map.empty, compile("tx.amount")).explicitGet() shouldBe 2 + 2
  }

  property("evaluates let statement lazily") {
    val eager = "let t = 1+1; t"
    ScriptEstimator(FunctionCosts, compile(eager)).explicitGet() shouldBe 5 + 102 + 2

    val lzy = "let t = 1+1; 2" // `t` is unused
    ScriptEstimator(FunctionCosts, compile(lzy)).explicitGet() shouldBe 5 + 1

    val onceOnly = "let x = 2+2; let y = x-x; x-y" // evaluated once only
    ScriptEstimator(FunctionCosts, compile(onceOnly)).explicitGet() shouldBe (5 + 102) + (5 + 14) + 14
  }

  property("ignores unused let statements") {
    val script = "let a = 1+2; let b = 2; let c = a+b; b" // `a` and `c` are unused
    ScriptEstimator(FunctionCosts, compile(script)).explicitGet() shouldBe 5 + (5 + 1) + 5 + 2
  }

  property("recursive let statement") {
    // let v = v; v
    val expr = BLOCK(LET("v", REF("v")), REF("v"))
    ScriptEstimator(Map.empty, expr) shouldBe 'right
  }

  property("evaluates if statement lazily") {
    val script = "let a = 1+2; let b = 3+4; let c = if (tx.amount > 5) then a else b; c"
    ScriptEstimator(FunctionCosts, compile(script)).explicitGet() shouldBe (5 + 102) + 5 + (5 + 16 + 2) + 2
  }
}
