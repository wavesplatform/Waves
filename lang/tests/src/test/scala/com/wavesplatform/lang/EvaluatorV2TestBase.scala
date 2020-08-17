package com.wavesplatform.lang

import cats.implicits._
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values.{Account, DApp, V4}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.{Decompiler, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class EvaluatorV2TestBase extends PropSpec with PropertyChecks with ScriptGen with Matchers with NoShrink with Inside {
  private val version = V4
  private val ctx =
    PureContext.build(version).withEnvironment[Environment] |+|
    WavesContext.build(DirectiveSet(version, Account, DApp).explicitGet())

  private val environment = Common.emptyBlockchainEnvironment()
  private val evaluator =
    new EvaluatorV2(LoggedEvaluationContext(_ => _ => (), ctx.evaluationContext(environment)), version)

  protected def evalExpr(expr: EXPR, limit: Int, evaluateAll: Boolean = true): (EXPR, String, Int) = {
    val (result, unusedComplexity) = evaluator(expr, limit, evaluateAll)
    (result, Decompiler(result, ctx.decompilerContext), limit - unusedComplexity)
  }

  protected def eval(script: String, limit: Int, evaluateAll: Boolean = true): (EXPR, String, Int) =
    evalExpr(compile(script), limit, evaluateAll)

  protected def compile(script: String): EXPR = {
    val parsed = Parser.parseExpr(script).get.value
    ExpressionCompiler(ctx.compilerContext, parsed).explicitGet()._1
  }
}
