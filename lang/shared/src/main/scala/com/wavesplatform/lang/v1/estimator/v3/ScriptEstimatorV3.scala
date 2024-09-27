package com.wavesplatform.lang.v1.estimator.v3

import cats.implicits.{toBifunctorOps, toFoldableOps, toTraverseOps}
import cats.syntax.functor.*
import cats.{Id, Monad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.User
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
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
    evalExpr(expr, Set(), globalDeclarationsMode).run(EstimatorContext(ctxFuncs)).value
  }

  private def evalExpr(t: EXPR, activeFuncArgs: Set[String], globalDeclarationsMode: Boolean = false): EvalM[Long] =
    if (Thread.currentThread().isInterrupted)
      raiseError("Script estimation was interrupted")
    else
      t match {
        case LET_BLOCK(let, inner)       => evalLetBlock(let, inner, activeFuncArgs, globalDeclarationsMode)
        case BLOCK(let: LET, inner)      => evalLetBlock(let, inner, activeFuncArgs, globalDeclarationsMode)
        case BLOCK(f: FUNC, inner)       => evalFuncBlock(f, inner, activeFuncArgs, globalDeclarationsMode)
        case BLOCK(_: FAILED_DEC, _)     => zero
        case REF(str)                    => evalRef(str, activeFuncArgs)
        case _: EVALUATED                => const(overheadCost)
        case IF(cond, t1, t2)            => evalIF(cond, t1, t2, activeFuncArgs)
        case GETTER(expr, _)             => evalGetter(expr, activeFuncArgs)
        case FUNCTION_CALL(header, args) => evalFuncCall(header, args, activeFuncArgs)
        case _: FAILED_EXPR              => zero
      }

  private def evalLetBlock(let: LET, nextExpr: EXPR, activeFuncArgs: Set[String], globalDeclarationsMode: Boolean): EvalM[Long] =
    for {
      _        <- if (globalDeclarationsMode) saveGlobalLetCost(let, activeFuncArgs) else doNothing
      startCtx <- get[Id, EstimatorContext, EstimationError]
      letEval = evalHoldingFuncs(let.value, activeFuncArgs, Some(startCtx.funcs))
      _            <- beforeNextExprEval(let, letEval)
      nextExprCost <- evalExpr(nextExpr, activeFuncArgs, globalDeclarationsMode)
      nextExprCtx  <- get[Id, EstimatorContext, EstimationError]
      _            <- afterNextExprEval(let, startCtx)
      letCost      <- if (nextExprCtx.usedRefs.contains(let.name)) letEval else const(0L)
      result       <- sum(nextExprCost, letCost)
    } yield result

  private def saveGlobalLetCost(let: LET, activeFuncArgs: Set[String]): EvalM[Unit] = {
    val costEvaluation =
      for {
        startCtx             <- get[Id, EstimatorContext, EstimationError]
        (bodyCost, usedRefs) <- withUsedRefs(evalExpr(let.value, activeFuncArgs))
        ctx                  <- get[Id, EstimatorContext, EstimationError]
        letCosts <- usedRefs.toSeq.traverse { ref =>
          local {
            for {
              _    <- update(ec => ec.copy(funcs = startCtx.funcs))
              cost <- ctx.globalLetEvals.getOrElse(ref, zero)
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

  private def beforeNextExprEval(let: LET, eval: EvalM[Long]): EvalM[Unit] =
    update(ctx => ctx.copy(usedRefs = ctx.usedRefs - let.name, refsCosts = ctx.refsCosts + (let.name -> local(eval))))

  private def afterNextExprEval(let: LET, startCtx: EstimatorContext): EvalM[Unit] =
    update(ctx =>
      ctx.copy(
        usedRefs = if (startCtx.usedRefs.contains(let.name)) ctx.usedRefs + let.name else ctx.usedRefs - let.name,
        refsCosts =
          if (startCtx.refsCosts.contains(let.name))
            ctx.refsCosts + (let.name -> startCtx.refsCosts(let.name))
          else
            ctx.refsCosts - let.name
      )
    )

  private def evalFuncBlock(func: FUNC, nextExpr: EXPR, activeFuncArgs: Set[String], globalDeclarationsMode: Boolean): EvalM[Long] =
    for {
      startCtx                   <- get[Id, EstimatorContext, EstimationError]
      _                          <- checkShadowing(func, startCtx)
      (funcCost, refsUsedInBody) <- withUsedRefs(evalHoldingFuncs(func.body, activeFuncArgs ++ func.args))
      _                          <- if (globalDeclarationsMode) saveGlobalFuncCost(func.name, funcCost, refsUsedInBody) else doNothing
      _                          <- handleUsedRefs(func.name, funcCost, startCtx, refsUsedInBody)
      nextExprCost               <- evalExpr(nextExpr, activeFuncArgs, globalDeclarationsMode)
    } yield nextExprCost

  private def checkShadowing(func: FUNC, startCtx: EstimatorContext): EvalM[Any] =
    if (fixOverflow && startCtx.funcs.contains(FunctionHeader.User(func.name)))
      raiseError(s"Function '${func.name}${func.args.mkString("(", ", ", ")")}' shadows preceding declaration")
    else
      doNothing

  private def saveGlobalFuncCost(name: String, funcCost: Long, refsUsedInBody: Set[String]): EvalM[Unit] =
    for {
      ctx      <- get[Id, EstimatorContext, EstimationError]
      letCosts <- local(refsUsedInBody.toSeq.traverse(ctx.globalLetEvals.getOrElse(_, zero)))
      totalCost = math.max(1, funcCost + letCosts.sum)
      _ <- set[Id, EstimatorContext, EstimationError](ctx.copy(globalFunctionsCosts = ctx.globalFunctionsCosts + (name -> totalCost)))
    } yield ()

  private def handleUsedRefs(name: String, cost: Long, startCtx: EstimatorContext, refsUsedInBody: Set[String]): EvalM[Unit] =
    update(ec =>
      ec.copy(
        funcs = ec.funcs + (User(name) -> (Coeval.now(cost), refsUsedInBody)),
        usedRefs = startCtx.usedRefs
      )
    )

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
      update(ec => ec.copy(usedRefs = ec.usedRefs + key)).map(_ => overheadCost)

  private def evalGetter(expr: EXPR, activeFuncArgs: Set[String]): EvalM[Long] =
    evalExpr(expr, activeFuncArgs).flatMap(sum(_, overheadCost))

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR], activeFuncArgs: Set[String]): EvalM[Long] =
    for {
      ctx                      <- get[Id, EstimatorContext, EstimationError]
      (bodyCost, bodyUsedRefs) <- getFuncCost(header, ctx)
      _                        <- setFuncToCtx(header, bodyCost, bodyUsedRefs)
      (argsCosts, _)           <- withUsedRefs(args.traverse(evalHoldingFuncs(_, activeFuncArgs)))
      argsCostsSum             <- argsCosts.foldM(0L)(sum)
      bodyCostV = bodyCost.value()
      correctedBodyCost <-
        if (bodyCostV != 0) const(bodyCostV)
        else if (overhead) zero
        else if (!overhead && !letFixes) const(1L)
        else isBlankFunc(bodyUsedRefs, ctx.refsCosts).map(if (_) 1L else 0L)
      result <- sum(argsCostsSum, correctedBodyCost)
    } yield result

  private def setFuncToCtx(header: FunctionHeader, bodyCost: Coeval[Long], bodyUsedRefs: Set[EstimationError]): EvalM[Unit] =
    update(ec =>
      ec.copy(
        funcs = ec.funcs + (header -> (bodyCost, Set())),
        usedRefs = ec.usedRefs ++ bodyUsedRefs
      )
    )

  private def getFuncCost(header: FunctionHeader, ctx: EstimatorContext): EvalM[(Coeval[Long], Set[EstimationError])] =
    ctx.funcs
      .get(header)
      .map(const)
      .getOrElse(
        raiseError[Id, EstimatorContext, EstimationError, (Coeval[Long], Set[EstimationError])](s"function '$header' not found")
      )

  private def isBlankFunc(usedRefs: Set[String], refsCosts: Map[String, EvalM[Long]]): EvalM[Boolean] =
    usedRefs.toSeq
      .existsM(refsCosts.get(_).existsM(_.map(_ > 0)))
      .map(!_)

  private def evalHoldingFuncs(
      expr: EXPR,
      activeFuncArgs: Set[String],
      ctxFuncsOpt: Option[Map[FunctionHeader, (Coeval[Long], Set[String])]] = None
  ): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, EstimationError]
      _        <- ctxFuncsOpt.fold(doNothing.void)(ctxFuncs => update(ec => ec.copy(funcs = ctxFuncs)))
      cost     <- evalExpr(expr, activeFuncArgs)
      _        <- update(ec => ec.copy(funcs = startCtx.funcs))
    } yield cost

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

  private val doNothing: EvalM[Any] =
    const(())

  private val zero: EvalM[Long] =
    const(0L)

  private def sum(a: Long, b: Long): EvalM[Long] = {
    def r = if (fixOverflow) Math.addExact(a, b) else a + b
    liftEither(Try(r).toEither.leftMap(_ => "Illegal script"))
  }
}

object ScriptEstimatorV3 {
  val latest = ScriptEstimatorV3(fixOverflow = true, overhead = false, letFixes = true)
}
