package com.wavesplatform.lang.v2.estimator

import cats.implicits._
import cats.{Id, Monad}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v2.estimator.EstimatorV3Context.EvalM3
import com.wavesplatform.lang.v2.estimator.EstimatorV3Context.Lenses._
import monix.eval.Coeval

object ScriptEstimatorV3 extends ScriptEstimator {
  override def apply(
    vars:  Set[String],
    funcs: Map[FunctionHeader, Coeval[Long]],
    expr:  EXPR
  ): Either[ExecutionError, Long] = {
    val f = funcs.mapValues(_.value)
    evalExpr(expr).run(EstimatorV3Context(f)).value._2
  }

  private def evalExpr(t: EXPR): EvalM3[Long] =
    t match {
      case LET_BLOCK(let, inner)       => evalLetBlock(let, inner)
      case BLOCK(let: LET, inner)      => evalLetBlock(let, inner)
      case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner)
      case REF(str)                    => markRef(str)
      case _: EVALUATED                => const(1L)
      case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
      case GETTER(expr, _)             => evalGetter(expr)
      case FUNCTION_CALL(header, args) => evalFuncCall(header, args)
    }

  private def evalHoldingFuncs(expr: EXPR): EvalM3[Long] =
    for {
      startCtx <- get[Id, EstimatorV3Context, ExecutionError]
      cost     <- evalExpr(expr)
      _        <- update(funcs.set(_)(startCtx.funcs))
    } yield cost

  private def evalLetBlock(let: LET, inner: EXPR): EvalM3[Long] =
    for {
      startCtx <- get[Id, EstimatorV3Context, ExecutionError]
      overlap   = startCtx.usedRefs.contains(let.name)
      _        <- update(usedRefs.modify(_)(_ - let.name))
      letEval   = evalHoldingFuncs(let.value)
      nextCost <- evalExpr(inner)
      ctx      <- get[Id, EstimatorV3Context, ExecutionError]
      letCost  <- if (ctx.usedRefs.contains(let.name)) letEval else const(0L)
      _        <- update(usedRefs.modify(_)(r => if (overlap) r + let.name else r - let.name))
    } yield nextCost + letCost

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM3[Long] =
    for {
      funcCost <- evalHoldingFuncs(func.body)
      _        <- update(funcs.modify(_)(_ + (FunctionHeader.User(func.name) -> funcCost)))
      nextCost <- evalExpr(inner)
    } yield nextCost

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM3[Long] =
    for {
      startCtx <- get[Id, EstimatorV3Context, ExecutionError]
      cond     <- evalHoldingFuncs(cond)
      right    <- evalHoldingFuncs(ifTrue)
      left     <- evalHoldingFuncs(ifFalse)
      _        <- update(funcs.set(_)(startCtx.funcs))
    } yield cond + Math.max(right, left) + 1

  private def markRef(key: String): EvalM3[Long] =
    update(usedRefs.modify(_)(_ + key)).map(_ => 1)

  private def evalGetter(expr: EXPR): EvalM3[Long] =
    evalExpr(expr).map(_ + 1)

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR]): EvalM3[Long] =
    for {
      ctx      <- get[Id, EstimatorV3Context, ExecutionError]
      bodyCost <- funcs.get(ctx).get(header).map(const)
        .getOrElse(raiseError[Id, EstimatorV3Context, ExecutionError, Long](s"function '$header' not found"))
      argsCost <- args.traverse(evalHoldingFuncs)
    } yield argsCost.sum + bodyCost

  private def update(f: EstimatorV3Context => EstimatorV3Context): EvalM3[Unit] =
    modify[Id, EstimatorV3Context, ExecutionError](f)

  private def const[A](a: A): EvalM3[A] =
    Monad[EvalM3].pure(a)
}
