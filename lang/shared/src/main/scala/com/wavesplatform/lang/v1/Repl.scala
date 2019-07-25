package com.wavesplatform.lang.v1

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.{ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser
import monix.execution.atomic.Atomic
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.compiler.Types.{FINAL, UNIT}

case class Repl(ver: StdLibVersion = V3) {
  private val Global: BaseGlobal = com.wavesplatform.lang.Global
  private val ctx         = CryptoContext.build(Global, ver) |+| PureContext.build(Global, ver)
  private val scriptAcc   = Atomic("")

  def execute(expr: String): Either[String, String] =
    scriptAcc.transformAndExtract(acc => {
      val newScript = acc + "\n" + expr
      evalExpr(newScript) match {
        case Left(e)          => (Left(e), acc)
        case Right((r, UNIT)) => (Right(r), newScript)
        case Right((r, _))    => (Right(r), acc)
      }
    })

  private def evalExpr(expr: String): Either[String, (String, FINAL)] =
    for {
      parsed <- Parser.parseExprOrDecl(expr).fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result) }
      )
      (compiled, cType) <- ExpressionCompiler(ctx.compilerContext, parsed)
      eval              <- EvaluatorV1[EVALUATED](ctx.evaluationContext, compiled)
    } yield (eval.prettyString(0), cType)
}
