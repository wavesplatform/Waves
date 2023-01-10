package com.wavesplatform.lang.v1.estimator.v3

import cats.implicits.*
import cats.{Id, Monad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.Lenses.*
import com.wavesplatform.lang.v1.estimator.{EstimationError, ScriptEstimator}
import com.wavesplatform.lang.v1.task.imports.*
import monix.eval.Coeval

import scala.util.Try

case class ScriptEstimatorV3(fixOverflow: Boolean, overhead: Boolean, letFixes: Boolean) extends ScriptEstimator {
  private val overheadCost: Long = if (overhead) 1 else 0

  override val version: Int = 3

  override def apply(
      vars: Set[String],
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR
  ): Either[String, Long] = {
    val ctxFuncs = funcs.view.mapValues((_, Set[String]())).toMap
    evalExpr(expr, Set()).run(EstimatorContext(ctxFuncs)).value._2
  }

  private def evalExpr(t: EXPR, activeFuncArgs: Set[String]): EvalM[Long] =
    if (Thread.currentThread().isInterrupted)
      raiseError("Script estimation was interrupted")
    else
      t match {
        case LET_BLOCK(let, inner)       => evalLetBlock(let, inner, activeFuncArgs)
        case BLOCK(let: LET, inner)      => evalLetBlock(let, inner, activeFuncArgs)
        case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner, activeFuncArgs)
        case BLOCK(_: FAILED_DEC, _)     => const(0)
        case REF(str)                    => evalRef(str, activeFuncArgs)
        case _: EVALUATED                => const(overheadCost)
        case IF(cond, t1, t2)            => evalIF(cond, t1, t2, activeFuncArgs)
        case GETTER(expr, _)             => evalGetter(expr, activeFuncArgs)
        case FUNCTION_CALL(header, args) => evalFuncCall(header, args, activeFuncArgs)
        case _: FAILED_EXPR              => const(0)
      }

  private def evalHoldingFuncs(expr: EXPR, activeFuncArgs: Set[String]): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      cost     <- evalExpr(expr, activeFuncArgs)
      _        <- update(funcs.set(_)(startCtx.funcs))
    } yield cost

  private def evalLetBlock(let: LET, inner: EXPR, activeFuncArgs: Set[String]): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      letEval = evalHoldingFuncs(let.value, activeFuncArgs)
      _            <- beforeNextExprEval(let, letEval)
      nextExprCost <- evalExpr(inner, activeFuncArgs)
      nextExprCtx  <- get[Id, EstimatorContext, EstimationError]
      _            <- afterNextExprEval(let, startCtx)
      letCost      <- if (nextExprCtx.usedRefs.contains(let.name)) letEval else const(0L)
      result       <- sum(nextExprCost, letCost)
    } yield result

  private def beforeNextExprEval(let: LET, eval: EvalM[Long]): EvalM[Unit] =
    for {
      cost <- local(eval)
      _ <- update(ctx =>
        usedRefs
          .modify(ctx)(_ - let.name)
          .copy(refsCosts = ctx.refsCosts + (let.name -> cost))
      )
    } yield ()

  private def afterNextExprEval(let: LET, startCtx: EstimatorContext): EvalM[Unit] =
    update(ctx =>
      usedRefs
        .modify(ctx)(r => if (startCtx.usedRefs.contains(let.name)) r + let.name else r - let.name)
        .copy(refsCosts =
          if (startCtx.refsCosts.contains(let.name))
            ctx.refsCosts + (let.name -> startCtx.refsCosts(let.name))
          else
            ctx.refsCosts - let.name
        )
    )

  private def evalFuncBlock(func: FUNC, inner: EXPR, activeFuncArgs: Set[String]): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      _ <-
        if (fixOverflow && startCtx.funcs.contains(FunctionHeader.User(func.name)))
          raiseError(s"Function '${func.name}${func.args.mkString("(", ", ", ")")}' shadows preceding declaration"): EvalM[Long]
        else const(0L)
      (funcCost, usedRefsInBody) <- withUsedRefs(evalHoldingFuncs(func.body, activeFuncArgs ++ func.args))
      _ <- update(
        (funcs ~ usedRefs).modify(_) { case (funcs, _) =>
          (
            funcs + ((FunctionHeader.User(func.name), (Coeval.now(funcCost), usedRefsInBody))),
            startCtx.usedRefs
          )
        }
      )
      nextCost <- evalExpr(inner, activeFuncArgs)
    } yield nextCost

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, activeFuncArgs: Set[String]): EvalM[Long] =
    for {
      cond  <- evalHoldingFuncs(cond, activeFuncArgs)
      right <- evalHoldingFuncs(ifTrue, activeFuncArgs)
      left  <- evalHoldingFuncs(ifFalse, activeFuncArgs)
      r1    <- sum(cond, Math.max(right, left))
      r2    <- sum(r1, overheadCost)
    } yield r2

  private def evalRef(key: String, activeFuncArgs: Set[String]): EvalM[Long] =
    if (activeFuncArgs.contains(key) && letFixes)
      const(overheadCost)
    else
      update(usedRefs.modify(_)(_ + key)).map(_ => overheadCost)

  private def evalGetter(expr: EXPR, activeFuncArgs: Set[String]): EvalM[Long] =
    evalExpr(expr, activeFuncArgs).flatMap(sum(_, overheadCost))

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR], activeFuncArgs: Set[String]): EvalM[Long] =
    for {
      ctx <- get[Id, EstimatorContext, EstimationError]
      (bodyCost, bodyUsedRefs) <- funcs
        .get(ctx)
        .get(header)
        .map(const)
        .getOrElse(
          raiseError[Id, EstimatorContext, EstimationError, (Coeval[Long], Set[String])](s"function '$header' not found")
        )
      _ <- update(
        (funcs ~ usedRefs).modify(_) { case (funcs, usedRefs) =>
          (
            funcs + ((header, (bodyCost, Set[String]()))),
            usedRefs ++ bodyUsedRefs
          )
        }
      )
      (argsCosts, argsUsedRefs) <- withUsedRefs(args.traverse(evalHoldingFuncs(_, activeFuncArgs)))
      argsCostsSum              <- argsCosts.foldM(0L)(sum)
      bodyCostV = bodyCost.value()
      correctedBodyCost =
        if (!overhead && !letFixes && bodyCostV == 0) 1
        else if (letFixes && bodyCostV == 0 && isBlankFunc(bodyUsedRefs ++ argsUsedRefs, ctx.refsCosts)) 1
        else bodyCostV
      result <- sum(argsCostsSum, correctedBodyCost)
    } yield result

  private def isBlankFunc(usedRefs: Set[String], refsCosts: Map[String, Long]): Boolean =
    !usedRefs.exists(refsCosts.get(_).exists(_ > 0))

  private def withUsedRefs[A](eval: EvalM[A]): EvalM[(A, Set[String])] =
    for {
      ctxBefore <- get[Id, EstimatorContext, EstimationError]
      result    <- eval
      ctxAfter  <- get[Id, EstimatorContext, EstimationError]
    } yield (result, ctxAfter.usedRefs diff ctxBefore.usedRefs)

  private def update(f: EstimatorContext => EstimatorContext): EvalM[Unit] =
    modify[Id, EstimatorContext, EstimationError](f)

  private def const[A](a: A): EvalM[A] =
    Monad[EvalM].pure(a)

  private def sum(a: Long, b: Long): EvalM[Long] = {
    def r = if (fixOverflow) Math.addExact(a, b) else a + b
    liftEither(Try(r).toEither.leftMap(_ => "Illegal script"))
  }
}

object ScriptEstimatorV3 {
  val latest = ScriptEstimatorV3(fixOverflow = true, overhead = false, letFixes = true)
}
