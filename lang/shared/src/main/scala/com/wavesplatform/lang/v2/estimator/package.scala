package com.wavesplatform.lang.v2

import cats.Id
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, LET}
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v2.estimator.EstimatorContext.EvalM
import com.wavesplatform.lang.v2.estimator.EstimatorContext.Lenses._

package object estimator {
  object ScriptEstimatorV2 extends NewScriptEstimator {
    override def evalLetBlock(let: LET, inner: EXPR): EvalM[Long] =
      local {
        val letResult = (false, evalExpr(let.value))
        for {
          _ <- update(lets.modify(_)(_.updated(let.name, letResult)))
          r <- evalExpr(inner)
        } yield r + 5
      }

    override def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[Long] =
      for {
        condComplexity  <- evalExpr(cond)
        rightComplexity <- evalExpr(ifTrue)
        leftComplexity  <- evalExpr(ifFalse)
      } yield condComplexity + Math.max(leftComplexity, rightComplexity) + 1
  }

  object ScriptEstimatorV3 extends NewScriptEstimator {
    override def evalLetBlock(let: LET, inner: EXPR): EvalM[Long] = {
      val letResult = (false, evalExpr(let.value))
      for {
        _ <- update(lets.modify(_)(_.updated(let.name, letResult)))
        r <- evalExpr(inner)
      } yield r + 5
    }

    override def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[Long] =
      for {
        condComplexity             <- evalExpr(cond)
        right@(_, rightComplexity) <- local(withCtx(evalExpr(ifTrue)))
        left@(_, leftComplexity)   <- local(withCtx(evalExpr(ifFalse)))
        (newCtx, complexity) = if (rightComplexity > leftComplexity) right else left
        _ <- set[Id, EstimatorContext, ExecutionError](newCtx)
      } yield condComplexity + complexity + 1

    private def withCtx(eval: EvalM[Long]): EvalM[(EstimatorContext, Long)] =
      for {
        r   <- eval
        ctx <- get[Id, EstimatorContext, ExecutionError]
      } yield (ctx, r)
  }
}
