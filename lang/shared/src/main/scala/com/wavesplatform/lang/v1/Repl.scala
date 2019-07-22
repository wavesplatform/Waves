package com.wavesplatform.lang.v1

import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler, Terms, Types}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Expressions.PART.VALID
import com.wavesplatform.lang.v1.parser.Expressions.{BLOCK, EXPR, INVALID, REF}
import com.wavesplatform.lang.v1.parser.Parser
import monix.execution.atomic.Atomic
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED

case class Repl(ver: StdLibVersion = V3) {
  private val Global: BaseGlobal = com.wavesplatform.lang.Global
  private val initialCtx         = CryptoContext.build(Global, ver) |+| PureContext.build(Global, ver)
  private val scriptAcc          = Atomic("")

  def execute(expr: String): Either[String, String] =
    scriptAcc.transformAndExtract(acc => {
      val newScript = acc + "\n" + expr
      evalExpr(newScript) match {
        case Left(e)           => (Left(e), acc)
        case Right((r, false)) => (Right(r), acc)
        case Right((r, true))  => (Right(r), newScript)
      }
    })

  private def evalExpr(expr: String): Either[String, (String, Boolean)] =
    for {
      parsed <- Parser.parseExpr(expr).fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result)             }
      )
      (compiled, isDecl) <- compile(parsed)
      eval               <- EvaluatorV1[EVALUATED](initialCtx.evaluationContext, compiled)
    } yield (eval.prettyString(0), isDecl)

  private def compile(parsed: EXPR): Either[String, (Terms.EXPR, Boolean)] = {
    val (expr, isDecl) = resolveDeclaration(parsed)
    ExpressionCompiler(initialCtx.compilerContext, expr).map(r => (r._1, isDecl))
  }

  private def resolveDeclaration(parsed: EXPR): (EXPR, Boolean) =
    parsed match {
      case BLOCK(bpos, decl, b: BLOCK)        =>
        val (expr, isDecl) = resolveDeclaration(b)
        (BLOCK(bpos, decl, expr), isDecl)
      case BLOCK(bpos, decl, INVALID(pos, _)) => (BLOCK(bpos, decl, REF(pos, VALID(pos, "unit"))), true)
      case a                                  => (a, false)
    }
}
