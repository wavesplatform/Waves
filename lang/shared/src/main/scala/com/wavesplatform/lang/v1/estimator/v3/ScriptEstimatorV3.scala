package com.wavesplatform.lang.v1.estimator.v3

import cats.implicits._
import cats.{Id, Monad}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.Lenses._
import com.wavesplatform.lang.v1.task.imports._
import monix.eval.Coeval

object ScriptEstimatorV3 extends ScriptEstimator {
  override val version: Int = 3

  override def apply(
      vars: Set[String],
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR
  ): Either[ExecutionError, Long] = {
    val ctxFuncs = funcs.view.mapValues((_, Set[String]())).toMap
    evalExpr(expr).run(EstimatorContext(ctxFuncs)).value._2
  }

  private def evalExpr(t: EXPR): EvalM[Long] =
    if (Thread.currentThread().isInterrupted)
      raiseError("Script estimation was interrupted")
    else
      t match {
        case LET_BLOCK(let, inner)       => evalLetBlock(let, inner)
        case BLOCK(let: LET, inner)      => evalLetBlock(let, inner)
        case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner)
        case BLOCK(_: FAILED_DEC, _)     => const(0)
        case REF(str)                    => markRef(str)
        case _: EVALUATED                => const(1L)
        case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
        case GETTER(expr, _)             => evalGetter(expr)
        case FUNCTION_CALL(header, args) => evalFuncCall(header, args)
        case _: FAILED_EXPR              => const(0)
      }

  private def evalHoldingFuncs(expr: EXPR): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, ExecutionError]
      cost     <- evalExpr(expr)
      _        <- update(funcs.set(_)(startCtx.funcs))
    } yield cost

  private def evalLetBlock(let: LET, inner: EXPR): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, ExecutionError]
      overlap = startCtx.usedRefs.contains(let.name)
      _ <- update(usedRefs.modify(_)(_ - let.name))
      letEval = evalHoldingFuncs(let.value)
      nextCost <- evalExpr(inner)
      ctx      <- get[Id, EstimatorContext, ExecutionError]
      letCost  <- if (ctx.usedRefs.contains(let.name)) letEval else const(0L)
      _        <- update(usedRefs.modify(_)(r => if (overlap) r + let.name else r - let.name))
    } yield nextCost + letCost

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[Long] =
    for {
      startCtx    <- get[Id, EstimatorContext, ExecutionError]
      funcCost    <- evalHoldingFuncs(func.body)
      bodyEvalCtx <- get[Id, EstimatorContext, ExecutionError]
      usedRefsInBody = bodyEvalCtx.usedRefs diff startCtx.usedRefs
      _ <- update(
        (funcs ~ usedRefs).modify(_) {
          case (funcs, _) =>
            (
              funcs + ((FunctionHeader.User(func.name), (Coeval.now(funcCost), usedRefsInBody))),
              startCtx.usedRefs
            )
        }
      )
      nextCost <- evalExpr(inner)
    } yield nextCost

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[Long] =
    for {
      cond  <- evalHoldingFuncs(cond)
      right <- evalHoldingFuncs(ifTrue)
      left  <- evalHoldingFuncs(ifFalse)
    } yield cond + Math.max(right, left) + 1

  private def markRef(key: String): EvalM[Long] =
    update(usedRefs.modify(_)(_ + key)).map(_ => 1)

  private def evalGetter(expr: EXPR): EvalM[Long] =
    evalExpr(expr).map(_ + 1)

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR]): EvalM[Long] =
    for {
      ctx <- get[Id, EstimatorContext, ExecutionError]
      (bodyCost, bodyUsedRefs) <- funcs
        .get(ctx)
        .get(header)
        .map(const)
        .getOrElse(raiseError[Id, EstimatorContext, ExecutionError, (Coeval[Long], Set[String])](s"function '$header' not found"))
      _ <- update(
        (funcs ~ usedRefs).modify(_) {
          case (funcs, usedRefs) =>
            (
              funcs + ((header, (bodyCost, Set[String]()))),
              usedRefs ++ bodyUsedRefs
            )
        }
      )
      argsCost <- args.traverse(evalHoldingFuncs)
    } yield argsCost.sum + bodyCost.value()

  private def update(f: EstimatorContext => EstimatorContext): EvalM[Unit] =
    modify[Id, EstimatorContext, ExecutionError](f)

  private def const[A](a: A): EvalM[A] =
    Monad[EvalM].pure(a)
}
