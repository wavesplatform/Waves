package com.wavesplatform.lang.v1

import cats.Eval
import cats.data.EitherT
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext
import com.wavesplatform.lang.v1.task.TaskM
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}

package object evaluator {
  type EvalM[A] = TaskM[LoggedEvaluationContext, ExecutionError, A]

  implicit class EvalMOps[A](ev: EvalM[A]) {
    def ter(ctx: LoggedEvaluationContext): TrampolinedExecResult[A] = {
      EitherT(ev.run(ctx).map(_._2))
    }
  }

  def liftTER[A](ter: Eval[Either[ExecutionError, A]]): EvalM[A] = {
    TaskM(_ => ter)
  }

  type LetExecResult  = Either[ExecutionError, compiler.Terms.EVALUATED]
  type LogItem        = (String, LetExecResult)
  type Log            = List[LogItem]
  type LogCallback    = LetExecResult => Unit
  type LetLogCallback = String => LogCallback
}
