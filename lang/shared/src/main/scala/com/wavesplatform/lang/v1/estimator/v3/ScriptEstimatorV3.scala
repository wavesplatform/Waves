package com.wavesplatform.lang.v1.estimator.v3

import cats.implicits.*
import cats.{Id, Monad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.Lenses.*
import com.wavesplatform.lang.v1.estimator.{EstimationError, ScriptEstimator}
import com.wavesplatform.lang.v1.task.imports.*
import monix.eval.Coeval

import scala.util.Try

case class ScriptEstimatorV3(fixOverflow: Boolean, overhead: Boolean) extends ScriptEstimator {
  private val overheadCost: Long = if (overhead) 1 else 0

  override val version: Int = 3

  override def apply(
      vars: Set[String],
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR
  ): Either[String, Long] =
    estimate(funcs, expr, globalDeclarationsMode = false)._2

  def globalDeclarationsCosts(
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR
  ): Either[EstimationError, GlobalDeclarationsCosts] = {
    val result = estimate(funcs, expr, globalDeclarationsMode = true)
    result._2.map(_ => GlobalDeclarationsCosts(result._1.globalLetsCosts, result._1.globalFunctionsCosts))
  }

  private def estimate(
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR,
      globalDeclarationsMode: Boolean
  ): (EstimatorContext, Either[EstimationError, Long]) = {
    val ctxFuncs = funcs.view.mapValues((_, Set[String]())).toMap
    evalExpr(expr, globalDeclarationsMode).run(EstimatorContext(ctxFuncs)).value
  }

  private def evalExpr(t: EXPR, globalDeclarationsMode: Boolean = false): EvalM[Long] =
    if (Thread.currentThread().isInterrupted)
      raiseError("Script estimation was interrupted")
    else
      t match {
        case LET_BLOCK(let, inner)       => evalLetBlock(let, inner, globalDeclarationsMode)
        case BLOCK(let: LET, inner)      => evalLetBlock(let, inner, globalDeclarationsMode)
        case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner, globalDeclarationsMode)
        case BLOCK(_: FAILED_DEC, _)     => zero
        case REF(str)                    => markRef(str)
        case _: EVALUATED                => const(overheadCost)
        case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
        case GETTER(expr, _)             => evalGetter(expr)
        case FUNCTION_CALL(header, args) => evalFuncCall(header, args)
        case _: FAILED_EXPR              => zero
      }

  private def evalHoldingFuncs(expr: EXPR, ctxFuncsOpt: Option[Map[FunctionHeader, (Coeval[Long], Set[String])]] = None): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      _        <- ctxFuncsOpt.fold(doNothing.void)(ctxFuncs => update(funcs.set(_)(ctxFuncs)))
      cost     <- evalExpr(expr)
      _        <- update(funcs.set(_)(startCtx.funcs))
    } yield cost

  private def evalLetBlock(let: LET, inner: EXPR, globalDeclarationsMode: Boolean): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      overlap = startCtx.usedRefs.contains(let.name)
      _ <- update(usedRefs.modify(_)(_ - let.name))
      letEval = evalHoldingFuncs(let.value, Some(startCtx.funcs))
      _        <- if (globalDeclarationsMode) saveGlobalLetCost(let) else doNothing
      nextCost <- evalExpr(inner, globalDeclarationsMode)
      ctx      <- get[Id, EstimatorContext, EstimationError]
      letCost  <- if (ctx.usedRefs.contains(let.name)) letEval else zero
      _        <- update(usedRefs.modify(_)(r => if (overlap) r + let.name else r - let.name))
      result   <- sum(nextCost, letCost)
    } yield result

  private def saveGlobalLetCost(let: LET): EvalM[Unit] = {
    val costEvaluation =
      for {
        startCtx    <- get[Id, EstimatorContext, EstimationError]
        bodyCost    <- evalExpr(let.value)
        bodyEvalCtx <- get[Id, EstimatorContext, EstimationError]
        usedRefs = bodyEvalCtx.usedRefs diff startCtx.usedRefs
        letCosts <- usedRefs.toSeq.traverse { ref =>
          local {
            for {
              _    <- update(funcs.set(_)(startCtx.funcs))
              cost <- bodyEvalCtx.globalLetEvals.getOrElse(ref, zero)
            } yield cost
          }
        }
      } yield bodyCost + letCosts.sum
    for {
      cost <- local(costEvaluation)
      _ <- update(ctx =>
        ctx.copy(
          globalLetEvals = ctx.globalLetEvals + (let.name   -> costEvaluation),
          globalLetsCosts = ctx.globalLetsCosts + (let.name -> cost)
        )
      )
    } yield ()
  }

  private def evalFuncBlock(func: FUNC, inner: EXPR, globalDeclarationsMode: Boolean): EvalM[Long] =
    for {
      startCtx    <- get[Id, EstimatorContext, EstimationError]
      _           <- checkShadowing(func, startCtx)
      funcCost    <- evalHoldingFuncs(func.body)
      bodyEvalCtx <- get[Id, EstimatorContext, EstimationError]
      refsUsedInBody = bodyEvalCtx.usedRefs diff startCtx.usedRefs
      _        <- if (globalDeclarationsMode) saveGlobalFuncCost(func.name, funcCost, bodyEvalCtx, refsUsedInBody) else doNothing
      _        <- handleUsedRefs(func.name, funcCost, startCtx, refsUsedInBody)
      nextCost <- evalExpr(inner, globalDeclarationsMode)
    } yield nextCost

  private def checkShadowing(func: FUNC, startCtx: EstimatorContext): EvalM[Any] =
    if (fixOverflow && startCtx.funcs.contains(FunctionHeader.User(func.name)))
      raiseError(s"Function '${func.name}${func.args.mkString("(", ", ", ")")}' shadows preceding declaration")
    else
      doNothing

  private def saveGlobalFuncCost(name: String, funcCost: Long, ctx: EstimatorContext, refsUsedInBody: Set[String]): EvalM[Unit] =
    for {
      letCosts <- local(refsUsedInBody.toSeq.traverse(ctx.globalLetEvals.getOrElse(_, zero)))
      totalCost = math.max(1, funcCost + letCosts.sum)
      _ <- set[Id, EstimatorContext, EstimationError](ctx.copy(globalFunctionsCosts = ctx.globalFunctionsCosts + (name -> totalCost)))
    } yield ()

  private def handleUsedRefs(name: String, cost: Long, ctx: EstimatorContext, refsUsedInBody: Set[String]): EvalM[Unit] =
    update(
      (funcs ~ usedRefs).modify(_) { case (funcs, _) =>
        (
          funcs + (User(name) -> (Coeval.now(cost), refsUsedInBody)),
          ctx.usedRefs
        )
      }
    )

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[Long] =
    for {
      cond  <- evalHoldingFuncs(cond)
      right <- evalHoldingFuncs(ifTrue)
      left  <- evalHoldingFuncs(ifFalse)
      r1    <- sum(cond, Math.max(right, left))
      r2    <- sum(r1, overheadCost)
    } yield r2

  private def markRef(key: String): EvalM[Long] =
    update(usedRefs.modify(_)(_ + key)).map(_ => overheadCost)

  private def evalGetter(expr: EXPR): EvalM[Long] =
    evalExpr(expr).flatMap(sum(_, overheadCost))

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR]): EvalM[Long] =
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
      argsCosts    <- args.traverse(evalHoldingFuncs(_))
      argsCostsSum <- argsCosts.foldM(0L)(sum)
      bodyCostV         = bodyCost.value()
      correctedBodyCost = if (!overhead && bodyCostV == 0) 1 else bodyCostV
      result <- sum(argsCostsSum, correctedBodyCost)
    } yield result

  private def update(f: EstimatorContext => EstimatorContext): EvalM[Unit] =
    modify[Id, EstimatorContext, EstimationError](f)

  private def const[A](a: A): EvalM[A] =
    Monad[EvalM].pure(a)

  private val doNothing: EvalM[Any] =
    const(())

  private val zero: EvalM[Long] =
    const(0L)

  private def sum(a: Long, b: Long): EvalM[Long] = {
    def r = if (fixOverflow) Math.addExact(a, b) else a + b
    liftEither(Try(r).toEither.leftMap(_ => "Illegal script"))
  }
}
