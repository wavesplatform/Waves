package com.wavesplatform.lang.v1.evaluator.ctx

import cats.instances.either._
import cats.syntax.applicative._
import cats.syntax.flatMap._
import cats.{Eval, Monad, ~>}
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.LogCallback
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}

sealed trait LazyVal[F[_]] {
  val value: Eval[F[Either[ExecutionError, EVALUATED]]]

  def mapK[G[_]: Monad](f: F ~> G): LazyVal[G]
}

object LazyVal {
  private case class LazyValImpl[F[_]: Monad](
      v: Eval[F[Either[ExecutionError, EVALUATED]]],
      lc: LogCallback[F]
  ) extends LazyVal[F] {
    override val value: Eval[F[Either[ExecutionError, EVALUATED]]] =
      v.flatTap(a => Eval.now(lc(a))).memoize

    override def mapK[G[_]: Monad](f: F ~> G): LazyVal[G] =
      copy(v = v.map(inner => f(inner)), _ => Monad[F].unit)
  }

  def apply[F[_]: Monad](v: TrampolinedExecResult[F, EVALUATED], lc: LogCallback[F]): LazyVal[F] =
    LazyValImpl(v.value, lc)

  def apply[F[_]: Monad](v: TrampolinedExecResult[F, EVALUATED]): LazyVal[F] =
    LazyValImpl(v.value, _ => Monad[F].unit)

  def fromEval[F[_]: Monad](v: Eval[F[Either[ExecutionError, EVALUATED]]]): LazyVal[F] =
    LazyValImpl(v, _ => Monad[F].unit)

  def fromEvaluated[F[_]: Monad](v: EVALUATED, lc: LogCallback[F]): LazyVal[F] =
    LazyValImpl(v.pure[Either[ExecutionError, *]].pure[F].pure[Eval], lc)

  def fromEvaluated[F[_]: Monad](v: EVALUATED): LazyVal[F] =
    fromEvaluated(v, _ => Monad[F].unit)
}
