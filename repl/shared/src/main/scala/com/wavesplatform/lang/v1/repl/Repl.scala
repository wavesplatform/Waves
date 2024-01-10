package com.wavesplatform.lang.v1.repl

import cats.implicits.*
import cats.{Functor, Id}
import com.wavesplatform.lang.v1.compiler.CompilerContext
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.repl.node.ErrorMessageEnvironment
import com.wavesplatform.lang.v1.repl.node.http.WebEnvironment.executionContext
import com.wavesplatform.lang.v1.repl.node.http.{NodeClient, NodeConnectionSettings}
import monix.execution.atomic.Atomic

import scala.concurrent.Future

case class Repl(
    settings: Option[NodeConnectionSettings] = None,
    customHttpClient: Option[NodeClient] = None,
    libraries: List[String] = Nil,
    lastContext: (CompilerContext, EvaluationContext[Future]) = ???
//      (CTX.empty.compilerContext, Monoid[EvaluationContext[Future]].empty)
) {
//  private val environment = buildEnvironment(settings, customHttpClient)
  private val initialState: ((CompilerContext, EvaluationContext[scala.concurrent.Future]), StateView) = state(
    ??? /*(
      lastContext._1 |+| initialCtx.compilerContext,
      lastContext._2 |+| initialCtx.evaluationContext(environment)
    )*/,
    view
  )
  private val currentState = Atomic(initialState)
  private val engine       = new ReplEngine[Future]()

  if (libraries.nonEmpty) initLibraries()

  private def state[S, V](s: S, view: S => V): (S, V) = (s, view(s))
  private def view(ctx: (CompilerContext, Any))       = StateView(ctx._1)

  def clear(): Unit = currentState.set(initialState)

  def reconfigure(settings: NodeConnectionSettings): Repl =
    Repl(Some(settings), customHttpClient, libraries, currentState.get()._1)

  def info(str: String): String = currentState.get()._2.declMap(str)

  def totalInfo: String = currentState.get()._2.totalCtx

  def execute(expr: String): Future[Either[String, String]] =
    perform(
      currentState,
      view,
      (oldCtx: (CompilerContext, EvaluationContext[Future])) =>
        engine.eval(expr, version, oldCtx._1, oldCtx._2).map {
          case Left(e)            => (Left(e), oldCtx)
          case Right((r, newCtx)) => (Right(r), newCtx)
        }: Future[(Either[String, String], (CompilerContext, EvaluationContext[Future]))]
    )

  private def initLibraries(): Unit = {
    val libraryState = state(
      (
        initialCtx.compilerContext,
        initialCtx.evaluationContext(
          ErrorMessageEnvironment[Id]("Blockchain interaction using lets from libraries is prohibited, use functions instead")
        )
      ),
      view
    )
    perform[Id, (CompilerContext, EvaluationContext[Id]), Either[String, String], StateView](
      Atomic(libraryState),
      view,
      oldCtx =>
        new ReplEngine[Id]()
          .eval(libraries.mkString("\n"), version, oldCtx._1, oldCtx._2)
          .fold(
            e => throw new RuntimeException(e),
            { case (r, ctx @ (compilerCtx, evaluationCtx)) =>
              val mappedCtx = ???//evaluationCtx.mapK(λ[FunctionK[Id, Future]](Future.successful(_))) |+| initialCtx.evaluationContext(environment)
              currentState.set(state((compilerCtx, mappedCtx), view))
              (Right(r), ctx)
            }
          )
    )
  }

  private def perform[F[_]: Functor, S, R, V](
      value: Atomic[(S, V)],
      view: S => V,
      transition: S => F[(R, S)]
  ): F[R] = {
    val (current, _) = value.get()
    transition(current).map { case (result, next) =>
      value.set(state(next, view))
      result
    }
  }
}
