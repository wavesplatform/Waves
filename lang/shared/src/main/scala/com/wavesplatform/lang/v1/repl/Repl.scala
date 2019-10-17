package com.wavesplatform.lang.v1.repl

import cats.{Functor, Monoid}
import cats.implicits._
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.repl.node.http.NodeConnectionSettings
import com.wavesplatform.lang.v1.traits.Environment
import monix.execution.atomic.Atomic

import scala.concurrent.ExecutionContext.Implicits.{global => g}
import scala.concurrent.Future

case class Repl(
  settings: Option[NodeConnectionSettings] = None,
  lastСontext: (CompilerContext, EvaluationContext[Environment, Future]) =
    (CompilerContext.empty, Monoid[EvaluationContext[Environment, Future]].empty)
) {
  private val environment  = buildEnvironment(settings)
  private val initialState = state(
    (
      lastСontext._1 |+| initialCtx.compilerContext,
      lastСontext._2 |+| initialCtx.evaluationContext(environment)
    ),
    view
  )
  private val currentState = Atomic(initialState)
  private val engine = new ReplEngine[Future]()

  private def state[S, V](s: S, view: S => V): (S, V) = (s, view(s))
  private def view(ctx: (CompilerContext, EvaluationContext[Environment, Future])) = StateView(ctx._1)

  def clear(): Unit = currentState.set(initialState)

  def reconfigure(settings: NodeConnectionSettings): Repl =
    Repl(Some(settings), currentState.get()._1)

  def info(str: String): String = currentState.get()._2.declMap(str)

  def totalInfo: String = currentState.get()._2.totalCtx

  def execute(expr: String): Future[Either[String, String]] =
    perform(
      currentState,
      view,
      (oldCtx: (CompilerContext, EvaluationContext[Environment, Future])) =>
        engine.eval(expr, oldCtx._1, oldCtx._2).map {
          case Left(e)            => (Left(e),  oldCtx)
          case Right((r, newCtx)) => (Right(r), newCtx)
        }: Future[(Either[String, String], (CompilerContext, EvaluationContext[Environment, Future]))]
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
