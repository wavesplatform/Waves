package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.directives.DirectiveSet.contractDirectiveSet
import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.{CompilerContext, ExpressionCompiler}
import com.wavesplatform.lang.v1.evaluator.EvaluatorV1
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.waves.WavesContext
import com.wavesplatform.lang.v1.evaluator.ctx.impl.{CryptoContext, PureContext}
import com.wavesplatform.lang.v1.parser.Parser

import cats.implicits._
import monix.execution.atomic.Atomic

import scala.util.Try

case class Repl(ver: StdLibVersion = V3) {
  private val initialCtx =
    CryptoContext.build(Global, ver) |+|
    PureContext.build(Global, ver)   |+|
    WavesContext.build(contractDirectiveSet, failFastBlockchainEnv)

  private val initialState = state((initialCtx.compilerContext, initialCtx.evaluationContext), view)
  private val currentState = Atomic(initialState)

  private def state[S, V](s: S, view: S => V): (S, V) = (s, view(s))
  private def view(ctx: (CompilerContext, EvaluationContext)) = StateView(ctx._1, ctx._2)

  def clear(): Unit = currentState.set(initialState)

  def info(str: String): String = currentState.get()._2.declMap(str)

  def totalInfo: String = currentState.get()._2.totalCtx

  def execute(expr: String): Either[String, String] =
    perform(
      currentState,
      view,
      (oldCtx: (CompilerContext, EvaluationContext)) => {
        evalExpr(expr, oldCtx._1, oldCtx._2) match {
          case Left(e)            => (Left(e),  oldCtx)
          case Right((r, newCtx)) => (Right(r), newCtx)
        }
      }
    )

  private def perform[S, R, V](
    value:      Atomic[(S, V)],
    view:       S => V,
    transition: S => (R, S)
  ): R =
    value.transformAndExtract { case (current, _) =>
      val (result, next) = transition(current)
      (result, state(next, view))
    }

  private def evalExpr(
    expr:       String,
    compileCtx: CompilerContext,
    evalCtx:    EvaluationContext
  ): Either[String, (String, (CompilerContext, EvaluationContext))] =
    for {
      parsed <- Parser.parseExprOrDecl(expr).fold(
        { case (_, _, err) => Left(err.traced.toString) },
        { case (result, _) => Right(result) }
      )
      (newCompileCtx, compiled, _) <- tryEi(ExpressionCompiler.applyWithCtx(compileCtx, parsed))
      (newEvalCtx, eval)           <- tryEi(EvaluatorV1.applyWithCtx(evalCtx, compiled))
    } yield (eval.prettyString(0), (newCompileCtx, newEvalCtx))

  private def tryEi[R](r: => Either[String, R]): Either[String, R] =
    Try(r)
      .toEither
      .leftMap(e => if (e.getMessage != null) e.getMessage else e.toString)
      .flatten
}
