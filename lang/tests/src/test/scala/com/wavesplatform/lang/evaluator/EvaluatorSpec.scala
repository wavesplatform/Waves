package com.wavesplatform.lang.evaluator

import cats.Id
import cats.implicits.*
import com.wavesplatform.common.utils.EitherExt2
import com.wavesplatform.lang.directives.values.*
import com.wavesplatform.lang.directives.{DirectiveDictionary, DirectiveSet}
import com.wavesplatform.lang.miniev.{ComplexityLimit, Ev}
import com.wavesplatform.lang.utils.lazyContexts
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.{EVALUATED, EXPR}
import com.wavesplatform.lang.v1.evaluator.Log
import com.wavesplatform.lang.v1.parser.Parser.LibrariesOffset.NoLibraries
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.{Common, ExecutionError}
import com.wavesplatform.test.PropSpec
import org.scalatest.Inside
import org.scalatest.exceptions.TestFailedException

abstract class EvaluatorSpec extends PropSpec with ScriptGen with Inside {
  val lastVersion: StdLibVersion = DirectiveDictionary[StdLibVersion].all.last

  def eval(
      code: String
  )(implicit startVersion: StdLibVersion = V1, checkNext: Boolean = true, checkOldPowVersion: Boolean = false): Either[String, EVALUATED] =
    evalVerRange(code, startVersion, if (checkNext) lastVersion else startVersion, checkOldPowVersion)

  def evalVerRange(
      code: String,
      startVersion: StdLibVersion,
      endVersion: StdLibVersion,
      checkOldPowVersion: Boolean = false
  ): Either[String, EVALUATED] =
    evalInternal(compile(code, _), startVersion, endVersion, checkOldPowVersion).map(_._1)

  def evalExpr(expr: EXPR, startVersion: StdLibVersion, endVersion: StdLibVersion, checkOldPowVersion: Boolean = false): Either[String, EVALUATED] =
    evalInternal(_ => Right(expr), startVersion, endVersion, checkOldPowVersion).map(_._1)

  def evalWithCost(code: String)(implicit startVersion: StdLibVersion = V1): (EVALUATED, Int) = {
    val (result, unused) = evalInternal(compile(code, _), startVersion, lastVersion, checkOldPowVersion = false).explicitGet()
    (result, Int.MaxValue - unused)
  }

  private def evalInternal(
      toExpr: StdLibVersion => Either[String, EXPR],
      startVersion: StdLibVersion,
      endVersion: StdLibVersion,
      checkOldPowVersion: Boolean
  ): Either[String, (EVALUATED, Int)] = {
    def doEval(
        toExpr: StdLibVersion => Either[String, EXPR],
        version: StdLibVersion,
        useNewPowPrecision: Boolean
    ): Either[String, (EVALUATED, Int)] =
      for {
        compiled <- toExpr(version)
        (_, cost, result) = evalExpr(compiled, version, useNewPowPrecision)
        evaluated <- result.leftMap(_.message)
      } yield (evaluated, cost)

    val results = (for {
      version            <- DirectiveDictionary[StdLibVersion].all if version.id >= startVersion.id && version.id <= endVersion.id
      useNewPowPrecision <- if (checkOldPowVersion) Seq(false, true) else Seq(true)
      result = doEval(toExpr, version, useNewPowPrecision)
    } yield ((version, useNewPowPrecision), result)).toSeq.sortBy(_._1)

    val evaluatedResults = results.map { case (_, resultEi) => resultEi }

    if (evaluatedResults.map(_.map { case (evaluatedValue, _) => evaluatedValue }).distinct.size == 1)
      evaluatedResults.head
    else
      throw new TestFailedException(s"Evaluation results are not the same: $results", 0)
  }

  private def evalExpr(expr: EXPR, version: StdLibVersion, useNewPowPrecision: Boolean): (Log[Id], Int, Either[ExecutionError, EVALUATED]) = {
    val ctx     = lazyContexts((DirectiveSet(version, Account, Expression).explicitGet(), useNewPowPrecision, true)).value()
    val evalCtx = ctx.evaluationContext(Common.emptyBlockchainEnvironment())
    Ev.run(expr, evalCtx, ComplexityLimit.Unlimited, newMode = true, version)
  }

  private def compile(code: String, version: StdLibVersion): Either[String, EXPR] = {
    val ctx = lazyContexts((DirectiveSet(version, Account, Expression).explicitGet(), true, true)).value()
    ExpressionCompiler.compile(code, NoLibraries, ctx.compilerContext, version, allowIllFormedStrings = true).map(_._1)
  }
}
