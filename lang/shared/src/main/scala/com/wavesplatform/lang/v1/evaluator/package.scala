package com.wavesplatform.lang.v1

import cats.Eval
import cats.data.EitherT
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.task.TaskMT
import com.wavesplatform.lang.{EvalF, ExecutionError, TrampolinedExecResult}

package object evaluator {
  type EvalM[F[_], C, A] = TaskMT[F, LoggedEvaluationContext[F, C], ExecutionError, A]

  implicit class EvalMOps[F[_], C, A](ev: EvalM[F, C, A]) {
    def ter(ctx: LoggedEvaluationContext[F, C]): TrampolinedExecResult[F, A] =
      EitherT[EvalF[F, ?], ExecutionError, A](ev.run(ctx).map(_._2))
  }

  def liftTER[F[_], C, A](ter: Eval[F[Either[ExecutionError, A]]]): EvalM[F, C, A] =
    TaskMT(_ => ter)

  type LetExecResult[F[_]]  = F[Either[ExecutionError, compiler.Terms.EVALUATED]]
  type LogItem[F[_]]        = (String, LetExecResult[F])
  type Log[F[_]]            = List[LogItem[F]]
  type LogCallback[F[_]]    = LetExecResult[F] => Unit
  type LetLogCallback[F[_]] = String => LogCallback[F]
}
