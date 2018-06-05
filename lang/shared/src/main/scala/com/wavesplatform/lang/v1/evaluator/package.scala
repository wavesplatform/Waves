package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.{ExecutionError, TrampolinedExecResult}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext
import com.wavesplatform.lang.v1.task.TaskM
import monix.eval.Coeval

package object evaluator {
  type EvalM[A] = TaskM[EvaluationContext, ExecutionError, A]

  implicit class EvalMOps[A](ev: EvalM[A]) {
    def ter(ctx: EvaluationContext): TrampolinedExecResult[A] = {
      EitherT(ev.run(ctx).map(_._2))
    }
  }

  def liftTER[A](ter: Coeval[Either[ExecutionError, A]]): EvalM[A] = {
    TaskM(_ => ter)
  }
}
