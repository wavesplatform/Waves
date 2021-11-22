package com.wavesplatform.lang.evaluator

import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.Common
import com.wavesplatform.lang.directives.values._
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.parser.Parser
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.test.PropSpec
import org.scalatest.Inside
import org.scalatest.exceptions.TestFailedException

abstract class EvaluatorSpec extends PropSpec with ScriptGen with Inside {
  val lastVersion: StdLibVersion = DirectiveDictionary[StdLibVersion].all.last

  def eval(code: String)(implicit startVersion: StdLibVersion = V1, checkNext: Boolean = true): Either[String, EVALUATED] =
    eval(code, startVersion, if (checkNext) lastVersion else startVersion)

  def eval(code: String, startVersion: StdLibVersion, endVersion: StdLibVersion): Either[String, EVALUATED] =
    eval(compile(code, _), startVersion, endVersion)

  def eval(expr: EXPR, startVersion: StdLibVersion, endVersion: StdLibVersion): Either[String, EVALUATED] = {
    eval(_ => Right(expr), startVersion, endVersion)
  }

  private def eval[A](
      toExpr: StdLibVersion => Either[String, EXPR],
      startVersion: StdLibVersion,
      endVersion: StdLibVersion
  ): Either[String, EVALUATED] = {
    val results = DirectiveDictionary[StdLibVersion].all
      .filter(v => v.id >= startVersion.id && v.id <= endVersion.id)
      .toList
      .map(version => (version, toExpr(version).flatMap(r => eval(r, version))))
      .sortBy(_._1)
    if (results.map(_._2).distinct.size == 1)
      results.head._2
    else
      throw new TestFailedException(s"Evaluation results are not the same: $results", 0)
  }

  private def eval(expr: EXPR, version: StdLibVersion): Either[String, EVALUATED] = {
    val ctx     = lazyContexts(DirectiveSet(version, Account, Expression).explicitGet()).value()
    val evalCtx = ctx.evaluationContext(Common.emptyBlockchainEnvironment())
    EvaluatorV2.applyCompleted(evalCtx, expr, version, overhead = false)._3
  }

  private def compile(code: String, version: StdLibVersion): Either[String, EXPR] = {
    val ctx    = lazyContexts(DirectiveSet(version, Account, Expression).explicitGet()).value()
    val parsed = Parser.parseExpr(code).get.value
    ExpressionCompiler(ctx.compilerContext, parsed, allowIllFormedStrings = true).map(_._1)
  }
}
