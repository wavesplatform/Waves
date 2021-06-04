package com.wavesplatform.lang.evaluator

import com.wavesplatform.lang.Common
import com.wavesplatform.lang.Common.NoShrink
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{StdLibVersion, V1}
import com.wavesplatform.lang.v1.compiler.ExpressionCompiler
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.EvaluatorV2
import com.wavesplatform.lang.v1.evaluator.ctx.impl.PureContext
import com.wavesplatform.lang.v1.parser.{Expressions, Parser}
import com.wavesplatform.lang.v1.testing.ScriptGen
import com.wavesplatform.lang.v1.traits.Environment
import org.scalatest.{Inside, Matchers, PropSpec}
import org.scalatest.exceptions.TestFailedException
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

abstract class EvaluatorSpec extends PropSpec with ScalaCheckPropertyChecks with ScriptGen with Matchers with NoShrink with Inside {
  def eval(code: String)(implicit startVersion: StdLibVersion = V1, checkNext: Boolean = true): Either[String, EVALUATED] = {
    val parsedExpr = Parser.parseExpr(code).get.value
    val results = DirectiveDictionary[StdLibVersion].all
      .filter(v => if (checkNext) v.id >= startVersion.id else v.id == startVersion.id)
      .map(version => (version, eval(parsedExpr, version)))
      .toList
      .sortBy(_._1)
    if (results.map(_._2).distinct.size == 1)
      results.head._2
    else
      throw new TestFailedException(s"Evaluation results are not the same: $results", 0)
  }

  private def eval(parsedExpr: Expressions.EXPR, version: StdLibVersion): Either[String, EVALUATED] = {
    val ctx           = PureContext.build(version, fixUnicodeFunctions = true).withEnvironment[Environment]
    val typed         = ExpressionCompiler(ctx.compilerContext, parsedExpr, allowIllFormedStrings = true)
    val evaluationCtx = ctx.evaluationContext(Common.emptyBlockchainEnvironment())
    typed.flatMap(v => EvaluatorV2.applyCompleted(evaluationCtx, v._1, version)._3)
  }
}
