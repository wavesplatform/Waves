package com.wavesplatform.lang.v1

import cats.Eval
import cats.data.EitherT
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.task.TaskMT
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}

package object evaluator {
  type EvalM[F[_], A] = TaskMT[F, LoggedEvaluationContext[F], ExecutionError, A]

  implicit class EvalMOps[F[_], A](ev: EvalM[F, A]) {
    def ter(ctx: LoggedEvaluationContext[F]): TrampolinedExecResult[F, A] =
      EitherT[λ[q => Eval[F[q]]], ExecutionError, A](ev.run(ctx).map(_._2))
  }

  def liftTER[F[_], A](ter: Eval[F[Either[ExecutionError, A]]]): EvalM[F, A] =
    TaskMT(_ => ter)

  type LetExecResult[F[_]]  = F[Either[ExecutionError, compiler.Terms.EVALUATED]]
  type LogItem[F[_]]        = (String, LetExecResult[F])
  type Log[F[_]]            = List[LogItem[F]]
  type LogCallback[F[_]]    = LetExecResult[F] => Unit
  type LetLogCallback[F[_]] = String => LogCallback[F]
}
