package com.wavesplatform.lang.v1.repl

import cats.Functor
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.repl.http.NodeConnectionSettings
import monix.execution.atomic.Atomic

import scala.concurrent.ExecutionContext.Implicits.{global => g}
import scala.concurrent.Future

case class Repl(settings: Option[NodeConnectionSettings] = None) {
  private val initialCtx = buildInitialCtx(settings)
  private val initialState = state((initialCtx.compilerContext, initialCtx.evaluationContext), view)
  private val currentState = Atomic(initialState)
  private val engine = new ReplEngine[Future]()

  private def state[S, V](s: S, view: S => V): (S, V) = (s, view(s))
  private def view(ctx: (CompilerContext, EvaluationContext[Future])) = StateView(ctx._1)

  def clear(): Unit = currentState.set(initialState)

  def info(str: String): String = currentState.get()._2.declMap(str)

  def totalInfo: String = currentState.get()._2.totalCtx

  def execute(expr: String): Future[Either[String, String]] =
    perform(
      currentState,
      view,
      (oldCtx: (CompilerContext, EvaluationContext[Future])) =>
        engine.eval(expr, oldCtx._1, oldCtx._2).map {
          case Left(e)            => (Left(e),  oldCtx)
          case Right((r, newCtx)) => (Right(r), newCtx)
        }: Future[(Either[String, String], (CompilerContext, EvaluationContext[Future]))]
    )

  private def perform[F[_] : Functor, S, R, V](
    value:      Atomic[(S, V)],
    view:       S => V,
    transition: S => F[(R, S)]
  ): F[R] = {
    val (current, _) = value.get()
    transition(current).map { case (result, next) =>
      value.set(state(next, view))
      result
    }
  }
}
