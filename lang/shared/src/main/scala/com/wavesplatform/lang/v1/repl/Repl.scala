package com.wavesplatform.lang.v1.repl

import com.wavesplatform.lang.directives.values.{StdLibVersion, V3}
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import monix.execution.atomic.Atomic

case class Repl(ver: StdLibVersion = V3) {
  private val initialCtx = buildInitialCtx(ver)
  private val initialState = state((initialCtx.compilerContext, initialCtx.evaluationContext), view)
  private val currentState = Atomic(initialState)

  private def state[S, V](s: S, view: S => V): (S, V) = (s, view(s))
  private def view(ctx: (CompilerContext, EvaluationContext)) = StateView(ctx._1)

  def clear(): Unit = currentState.set(initialState)

  def info(str: String): String = currentState.get()._2.declMap(str)

  def totalInfo: String = currentState.get()._2.totalCtx

  def execute(expr: String): Either[String, String] =
    handle(
      currentState,
      view,
      (oldCtx: (CompilerContext, EvaluationContext)) =>
        ReplEngine.eval(expr, oldCtx._1, oldCtx._2) match {
          case Left(e)            => (Left(e),  oldCtx)
          case Right((r, newCtx)) => (Right(r), newCtx)
        }
    )

  private def handle[S, R, V](
    value:      Atomic[(S, V)],
    view:       S => V,
    transition: S => (R, S)
  ): R =
    value.transformAndExtract { case (current, _) =>
      val (result, next) = transition(current)
      (result, state(next, view))
    }
}
