package com.wavesplatform.lang.v1.estimator.v3

import cats.implicits._
import cats.{Id, Monad, catsInstancesForId}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.ScriptEstimator
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.Lenses._
import com.wavesplatform.lang.v1.task.imports._
import monix.eval.Coeval

class GenericScriptEstimatorV3(private val continuationFirstStepMode: Boolean) extends ScriptEstimator {
  override val version: Int = 3

  override def apply(
      vars: Set[String],
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR
  ): Either[ExecutionError, Long] = {
    val ctxFuncs =
      funcs.view.map {
        case (header, cost) =>
          val nativeCost = if (header.isExternal) cost.value() else 0
          header -> FunctionInfo(cost.value(), Set[String](), nativeCost)
      }.toMap
    evalExpr(expr, this.continuationFirstStepMode).run(EstimatorContext(ctxFuncs)).value._2
  }

  protected def evalExpr(t: EXPR, checkContinuationFirstStep: Boolean): EvalM[Long] = {
    val localCheckContinuationFirstStep =
      checkContinuationFirstStep && !(t.isInstanceOf[FUNCTION_CALL] && t.asInstanceOf[FUNCTION_CALL].function.isExternal)
    if (Thread.currentThread().isInterrupted)
      raiseError("Script estimation was interrupted")
    else {
      t match {
        case LET_BLOCK(let, inner)       => evalLetBlock(let, inner, localCheckContinuationFirstStep)
        case BLOCK(let: LET, inner)      => evalLetBlock(let, inner, localCheckContinuationFirstStep)
        case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner, localCheckContinuationFirstStep)
        case BLOCK(_: FAILED_DEC, _)     => const(0L)
        case REF(str)                    => markRef(str, localCheckContinuationFirstStep)
        case _: EVALUATED                => if (this.continuationFirstStepMode) const(0L) else const(1L)
        case IF(cond, t1, t2)            => evalIF(cond, t1, t2, localCheckContinuationFirstStep)
        case GETTER(expr, _)             => evalGetter(expr, localCheckContinuationFirstStep)
        case FUNCTION_CALL(header, args) => evalFuncCall(header, args, localCheckContinuationFirstStep)
        case _: FAILED_EXPR              => const(0L)
      }
    }
  }

  private def evalHoldingFuncs(expr: EXPR, checkContinuationFirstStep: Boolean): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, ExecutionError]
      cost     <- evalExpr(expr, checkContinuationFirstStep)
      _        <- update(funcs.set(_)(startCtx.funcs))
    } yield cost

  private def evalLetBlock(let: LET, inner: EXPR, checkContinuationFirstStep: Boolean): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, ExecutionError]
      overlap = startCtx.usedRefs.contains(let.name)
      _ <- update(usedRefs.modify(_)(_ - let.name))
      letEval = evalHoldingFuncs(let.value, checkContinuationFirstStep)
      nextCost <- evalExpr(inner, checkContinuationFirstStep)
      ctx      <- get[Id, EstimatorContext, ExecutionError]
      letCost  <- if (ctx.usedRefs.contains(let.name)) letEval else const(0L)
      _        <- update(usedRefs.modify(_)(r => if (overlap) r + let.name else r - let.name))
    } yield nextCost + letCost

  private def evalFuncBlock(func: FUNC, inner: EXPR, checkContinuationFirstStep: Boolean): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, ExecutionError]
      (cost, nativeCost) <- for {
        cost <- evalHoldingFuncs(func.body, checkContinuationFirstStep = false)
        nativeCost <- if (this.continuationFirstStepMode)
          local(evalExpr(func.body, checkContinuationFirstStep = true))
        else
          const(cost)
      } yield (cost, nativeCost)
      bodyEvalCtx <- get[Id, EstimatorContext, ExecutionError]
      usedRefsInBody = bodyEvalCtx.usedRefs diff startCtx.usedRefs
      _ <- update(
        (funcs ~ usedRefs).modify(_) {
          case (funcs, _) =>
            (
              funcs + ((FunctionHeader.User(func.name), FunctionInfo(cost, usedRefsInBody, nativeCost))),
              startCtx.usedRefs
            )
        }
      )
      nextCost <- evalExpr(inner, checkContinuationFirstStep)
    } yield nextCost

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, checkContinuationFirstStep: Boolean): EvalM[Long] =
    for {
      cond  <- evalHoldingFuncs(cond, checkContinuationFirstStep)
      right <- evalHoldingFuncs(ifTrue, checkContinuationFirstStep)
      left  <- evalHoldingFuncs(ifFalse, checkContinuationFirstStep)
      thenCost = if (checkContinuationFirstStep)
        right + left
      else
        Math.max(right, left) + 1
    } yield cond + thenCost

  private def markRef(key: String, checkContinuationFirstStep: Boolean): EvalM[Long] =
    update(usedRefs.modify(_)(_ + key))
      .map(_ => if (checkContinuationFirstStep) 0 else 1)

  private def evalGetter(expr: EXPR, checkContinuationFirstStep: Boolean): EvalM[Long] =
    evalExpr(expr, checkContinuationFirstStep)
      .map(_ + (if (checkContinuationFirstStep) 0 else 1))

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR], checkContinuationFirstStep: Boolean): EvalM[Long] =
    for {
      ctx <- get[Id, EstimatorContext, ExecutionError]
      FunctionInfo(bodyCost, bodyUsedRefs, bodyNativeCost) <- funcs
        .get(ctx)
        .get(header)
        .map(const)
        .getOrElse(raiseError[Id, EstimatorContext, ExecutionError, FunctionInfo](s"function '$header' not found"))
      _ <- update(
        (funcs ~ usedRefs).modify(_) {
          case (funcs, usedRefs) =>
            (
              funcs + ((header, FunctionInfo(bodyCost, Set[String](), bodyNativeCost))),
              usedRefs ++ bodyUsedRefs
            )
        }
      )
      resultBodyCost = if (!checkContinuationFirstStep || header.isExternal)
        bodyCost
      else
        bodyNativeCost
      argsCost <- args.traverse(evalHoldingFuncs(_, checkContinuationFirstStep && resultBodyCost == 0))

    } yield {
      argsCost.sum + resultBodyCost
    }

  private def update(f: EstimatorContext => EstimatorContext): EvalM[Unit] =
    modify[Id, EstimatorContext, ExecutionError](f)

  private def const[A](a: A): EvalM[A] =
    Monad[EvalM].pure(a)
}
