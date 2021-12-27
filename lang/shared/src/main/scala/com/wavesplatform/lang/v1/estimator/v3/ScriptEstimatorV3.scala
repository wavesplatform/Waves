package com.wavesplatform.lang.v1.estimator.v3

import cats.implicits._
import cats.{Id, Monad}
import com.wavesplatform.lang.ExecutionError
import com.wavesplatform.lang.directives.DirectiveDictionary
import com.wavesplatform.lang.directives.values.{Expression, StdLibVersion}
import com.wavesplatform.lang.utils.getDecompilerContext
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.FunctionHeader.{Native, User}
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.estimator.v3.EstimatorContext.Lenses._
import com.wavesplatform.lang.v1.estimator.{HighOrderFunctionInfo, ScriptEstimator}
import com.wavesplatform.lang.v1.task.imports._
import monix.eval.Coeval

import scala.util.Try

case class ScriptEstimatorV3(fixOverflow: Boolean) extends ScriptEstimator {
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
      result   <- sum(nextCost, letCost)
    } yield result

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[Long] =
    for {
      startCtx <- get[Id, EstimatorContext, ExecutionError]
      _ <- if (fixOverflow && startCtx.funcs.contains(FunctionHeader.User(func.name)))
        raiseError(s"Function '${func.name}${func.args.mkString("(", ", ", ")")}' shadows preceding declaration"): EvalM[Long]
      else const(0L)
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
      r1    <- sum(cond, Math.max(right, left))
      r2    <- sum(r1, 1)
    } yield r2

  private def markRef(key: String): EvalM[Long] =
    update(usedRefs.modify(_)(_ + key)).map(_ => 1)

  private def evalGetter(expr: EXPR): EvalM[Long] =
    evalExpr(expr).flatMap(sum(_, 1))

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR]): EvalM[Long] =
    for {
      ctx <- get[Id, EstimatorContext, ExecutionError]
      (bodyCost, bodyUsedRefs) <- funcs
        .get(ctx)
        .get(header)
        .map(const)
        .getOrElse(
          raiseError[Id, EstimatorContext, ExecutionError, (Coeval[Long], Set[String])](s"function '$header' not found")
        )
      internalCallsCost <- evalHighOrderFunc(ctx, header, args)
      _ <- update(
        (funcs ~ usedRefs).modify(_) {
          case (funcs, usedRefs) =>
            (
              funcs + ((header, (bodyCost, Set[String]()))),
              usedRefs ++ bodyUsedRefs
            )
        }
      )
      argsCosts    <- args.traverse(evalHoldingFuncs)
      argsCostsSum <- argsCosts.foldM(0L)(sum)
      bodyWithArgs <- sum(argsCostsSum, bodyCost.value())
      result <- sum(internalCallsCost, bodyWithArgs)
    } yield result

  private def evalHighOrderFunc(ctx: EstimatorContext, header: FunctionHeader, args: List[EXPR]): EvalM[Long] = {
    def errorPrefix = {
      val functionName =
        header match {
          case Native(id) =>
            val version = DirectiveDictionary[StdLibVersion].all.last
            getDecompilerContext(version, Expression).opCodes.getOrElse(id, header.toString)
          case u: User =>
            u.name
        }
      s"Unexpected call of high-order function $functionName: "
    }
    val r = HighOrderFunctionInfo.all
      .get(header)
      .map(
        info =>
          args
            .lift(info.functionIndex)
            .toRight(s"${errorPrefix}only ${args.size} args passed while ${info.functionIndex + 1} expected")
            .flatMap {
              case CONST_STRING(function) =>
                ctx.funcs
                  .get(User(function))
                  .map { case (complexity, _) => complexity.value() * info.callLimit }
                  .toRight(s"$errorPrefix'$function' is not found in the scope")
              case expr =>
                Left(s"${errorPrefix}expression '$expr' is passed as function reference")
            }
      )
      .getOrElse(Right(0L))
    liftEither(r)
  }

  private def update(f: EstimatorContext => EstimatorContext): EvalM[Unit] =
    modify[Id, EstimatorContext, ExecutionError](f)

  private def const[A](a: A): EvalM[A] =
    Monad[EvalM].pure(a)

  private def sum(a: Long, b: Long): EvalM[Long] = {
    def r = if (fixOverflow) Math.addExact(a, b) else a + b
    liftEither(Try(r).toEither.leftMap(_ => "Illegal script"))
  }
}
