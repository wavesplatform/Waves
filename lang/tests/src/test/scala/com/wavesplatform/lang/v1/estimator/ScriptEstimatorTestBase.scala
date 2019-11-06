package com.wavesplatform.lang.v1.estimator

import cats.kernel.Monoid
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common._
import com.wavesplatform.lang.directives.DirectiveSet
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.Contextful.NoContext
import com.wavesplatform.lang.v1.evaluator.ContextfulVal
import com.wavesplatform.lang.v1.evaluator.FunctionIds._
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.{Types, WavesContext}
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.v1.{CTX, FunctionHeader}
import com.wavesplatform.lang.{Common, Global, utils}
import monix.eval.Coeval
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks => PropertyChecks}

class ScriptEstimatorTestBase(estimators: ScriptEstimator*)
  extends PropSpec
     with PropertyChecks
     with Matchers
     with ScriptGen
     with NoShrink {

  val Plus  = FunctionHeader.Native(SUM_LONG)
  val Minus = FunctionHeader.Native(SUB_LONG)
  val Gt    = FunctionHeader.Native(GT_LONG)

  val customFunctionCosts: Map[FunctionHeader, Coeval[Long]] =
    Map[FunctionHeader, Long](Plus -> 100, Minus -> 10, Gt -> 10).mapValues(Coeval.now)

  private val v3FunctionCosts = utils.functionCosts(V3)

  private val ctx = {
    val transactionType = Types.buildTransferTransactionType(true)
    val tx              = CaseObj(transactionType, Map("amount" -> CONST_LONG(100000000L)))
    Monoid
      .combineAll(Seq(
        PureContext.build(Global, V3).withEnvironment[Environment],
        CryptoContext.build(Global, V3).withEnvironment[Environment],
        WavesContext.build(DirectiveSet.contractDirectiveSet),
        CTX[NoContext](
          Seq(transactionType),
          Map(("tx", (transactionType, ContextfulVal.pure[NoContext](tx)))),
          Array.empty
        ).withEnvironment[Environment]
      ))
  }

  private val env = Common.emptyBlockchainEnvironment()
  private val lets: Set[String] =
    ctx.evaluationContext(env).letDefs.keySet

  protected def compile(code: String): EXPR = {
    val untyped = Parser.parseExpr(code).get.value
    ExpressionCompiler(ctx.compilerContext, untyped).map(_._1).explicitGet()
  }

  protected def estimate(
    functionCosts: Map[FunctionHeader, Coeval[Long]] = v3FunctionCosts,
    script: EXPR
  ): Either[String, Long] = {
    val results = estimators.map(_(lets, functionCosts, script))
    if (results.distinct.length == 1)
      results.head
    else
      Left(s"Estimators discrepancy: ${results.toString}")
  }

  protected def estimateDelta(
    script1: EXPR,
    script2: EXPR,
    functionCosts: Map[FunctionHeader, Coeval[Long]] = v3FunctionCosts
  ): Either[String, Long] = {
    val results = estimators.map(
      e => for {
        cost2 <- e(lets, functionCosts, script2)
        cost1 <- e(lets, functionCosts, script1)
      } yield cost2 - cost1
    )
    if (results.distinct.length == 1)
      results.head
    else
      Left(s"Estimators discrepancy: ${results.toString}")
  }
}
