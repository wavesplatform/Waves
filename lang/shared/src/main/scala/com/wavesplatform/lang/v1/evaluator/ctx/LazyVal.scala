package com.wavesplatform.lang.v1.evaluator.ctx

import cats.{Eval, Monad}
import cats.implicits._
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}
import com.wavesplatform.lang.v1.compiler.Terms.EVALUATED
import com.wavesplatform.lang.v1.evaluator.LogCallback

sealed trait LazyVal[F[_]] {
  val value: Eval[F[Either[ExecutionError, EVALUATED]]]
}

object LazyVal {
  private case class LazyValImpl[F[_]](
    v: Eval[F[Either[ExecutionError, EVALUATED]]],
    lc: LogCallback[F]
  ) extends LazyVal[F] {
    override val value: Eval[F[Either[ExecutionError, EVALUATED]]] =
      v.flatTap(a => Eval.now(lc(a))).memoize
  }

  def apply[F[_]](v: TrampolinedExecResult[F, EVALUATED], lc: LogCallback[F]): LazyVal[F] =
    LazyValImpl(v.value, lc)

  def apply[F[_] : Monad](v: EVALUATED, lc: LogCallback[F]): LazyVal[F] =
    LazyValImpl(v.pure[Either[ExecutionError, ?]].pure[F].pure[Eval], lc)

  def apply[F[_] : Monad](v: TrampolinedExecResult[F, EVALUATED]): LazyVal[F] =
    LazyValImpl(v.value, _ => Monad[F].unit)
}
