package com.wavesplatform.lang.evaluator

import cats.Id
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.evaluator.{EvaluatorV2, Log}
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, ExecutionError}
import com.wavesplatform.test.PropSpec
import org.scalatest.Inside
import org.scalatest.exceptions.TestFailedException

abstract class EvaluatorSpec extends PropSpec with ScriptGen with Inside {
  val lastVersion: StdLibVersion = DirectiveDictionary[StdLibVersion].all.last

  def eval(code: String)(implicit startVersion: StdLibVersion = V1, checkNext: Boolean = true): Either[String, EVALUATED] =
    eval(code, startVersion, if (checkNext) lastVersion else startVersion)

  def eval(code: String, startVersion: StdLibVersion, endVersion: StdLibVersion): Either[String, EVALUATED] =
    eval(compile(code, _), startVersion, endVersion).map(_._1)

  def eval(expr: EXPR, startVersion: StdLibVersion, endVersion: StdLibVersion): Either[String, EVALUATED] =
    eval(_ => Right(expr), startVersion, endVersion).map(_._1)

  def evalWithCost(code: String)(implicit startVersion: StdLibVersion = V1): (EVALUATED, Int) = {
    val (result, unused) = eval(compile(code, _), startVersion, lastVersion).explicitGet()
    (result, Int.MaxValue - unused)
  }

  private def eval[A](
      toExpr: StdLibVersion => Either[String, EXPR],
      startVersion: StdLibVersion,
      endVersion: StdLibVersion
  ): Either[String, (EVALUATED, Int)] = {
    val results = DirectiveDictionary[StdLibVersion].all
      .filter(v => v.id >= startVersion.id && v.id <= endVersion.id)
      .toList
      .map(version => (version, eval(toExpr, version)))
      .sortBy(_._1)
    if (results.map { case (_, r) => r.map(_._1) }.distinct.size == 1)
      results.head._2
    else
      throw new TestFailedException(s"Evaluation results are not the same: $results", 0)
  }

  private def eval[A](toExpr: StdLibVersion => Either[String, EXPR], version: StdLibVersion): Either[String, (EVALUATED, Int)] =
    for {
      compiled <- toExpr(version)
      (_, cost, result) = evalExpr(compiled, version)
      evaluated <- result
    } yield (evaluated, cost)

  private def evalExpr(expr: EXPR, version: StdLibVersion): (Log[Id], Int, Either[ExecutionError, EVALUATED]) = {
    val ctx     = lazyContexts(DirectiveSet(version, Account, Expression).explicitGet()).value()
    val evalCtx = ctx.evaluationContext(Common.emptyBlockchainEnvironment())
    EvaluatorV2.applyCompleted(evalCtx, expr, version, correctFunctionCallScope = true, newMode = true)
  }

  private def compile(code: String, version: StdLibVersion): Either[String, EXPR] = {
    val ctx    = lazyContexts(DirectiveSet(version, Account, Expression).explicitGet()).value()
    val parsed = Parser.parseExpr(code).get.value
    ExpressionCompiler(ctx.compilerContext, parsed, allowIllFormedStrings = true).map(_._1)
  }
}
