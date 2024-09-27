package com.wavesplatform.lang.v1.estimator.v2

import cats.instances.list._
import cats.syntax.traverse._
import cats.{Id, Monad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.estimator.{EstimationError, ScriptEstimator}
import com.wavesplatform.lang.v1.estimator.v2.EstimatorContext.EvalM
import com.wavesplatform.lang.v1.task.imports._
import monix.eval.Coeval

object ScriptEstimatorV2 extends ScriptEstimator {
  override val version: Int = 2

  override def apply(
      vars: Set[String],
      funcs: Map[FunctionHeader, Coeval[Long]],
      expr: EXPR
  ): Either[EstimationError, Long] = {
    val v = vars.map((_, (true, const(0)))).toMap
    val f = funcs.view.mapValues(_.value()).toMap
    evalExpr(expr).run(EstimatorContext(v, f)).value._2
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
        case REF(str)                    => evalRef(str)
        case _: EVALUATED                => const(1)
        case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
        case GETTER(expr, _)             => evalGetter(expr)
        case FUNCTION_CALL(header, args) => evalFuncCall(header, args)
        case _: FAILED_EXPR              => const(0)
      }

  private def evalLetBlock(let: LET, inner: EXPR): EvalM[Long] =
    local {
      val letResult = (false, evalExpr(let.value))
      for {
        _ <- update(ec => ec.copy(letDefs = ec.letDefs.updated(let.name, letResult)))
        r <- evalExpr(inner)
      } yield r + 5
    }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[Long] =
    for {
      condComplexity  <- evalExpr(cond)
      rightComplexity <- evalExpr(ifTrue)
      leftComplexity  <- evalExpr(ifFalse)
    } yield condComplexity + Math.max(leftComplexity, rightComplexity) + 1

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[Long] =
    local {
      for {
        _ <- checkFuncCtx(func)
        _ <- update(ec => ec.copy(userFuncs = ec.userFuncs + (FunctionHeader.User(func.name) -> func)))
        r <- evalExpr(inner)
      } yield r + 5
    }

  private def checkFuncCtx(func: FUNC): EvalM[Unit] =
    local {
      for {
        _ <- update(ec => ec.copy(letDefs = ec.letDefs ++ func.args.map((_, (true, const(0)))).toMap))
        _ <- evalExpr(func.body)
      } yield ()
    }

  private def evalRef(key: String): EvalM[Long] =
    for {
      ctx <- get[Id, EstimatorContext, EstimationError]
      r <- ctx.letDefs.get(key) match {
        case Some((false, lzy)) => setRefEvaluated(key, lzy)
        case Some((true, _))    => const(0)
        case None               => raiseError[Id, EstimatorContext, EstimationError, Long](s"A definition of '$key' not found")
      }
    } yield r + 2

  private def setRefEvaluated(key: String, lzy: EvalM[Long]): EvalM[Long] =
    update(ec => ec.copy(letDefs = ec.letDefs.updated(key, (true, lzy))))
      .flatMap(_ => lzy)

  private def evalGetter(expr: EXPR): EvalM[Long] =
    evalExpr(expr).map(_ + 2)

  private def evalFuncCall(header: FunctionHeader, args: List[EXPR]): EvalM[Long] =
    for {
      ctx <- get[Id, EstimatorContext, EstimationError]
      bodyComplexity <- ctx.predefFuncs
        .get(header)
        .map(bodyComplexity => evalFuncArgs(args).map(_ + bodyComplexity))
        .orElse(ctx.userFuncs.get(header).map(evalUserFuncCall(_, args)))
        .getOrElse(raiseError[Id, EstimatorContext, EstimationError, Long](s"function '$header' not found"))
    } yield bodyComplexity

  private def evalUserFuncCall(func: FUNC, args: List[EXPR]): EvalM[Long] =
    for {
      argsComplexity <- evalFuncArgs(args)
      ctx            <- get[Id, EstimatorContext, EstimationError]
      _              <- update(ec => ec.copy(letDefs = ec.letDefs ++ ctx.overlappedRefs))
      overlapped = func.args.flatMap(arg => ctx.letDefs.get(arg).map((arg, _))).toMap
      ctxArgs    = func.args.map((_, (false, const(1)))).toMap
      _ <- update(ec =>
        ec.copy(
          letDefs = ec.letDefs ++ ctxArgs,
          overlappedRefs = ec.overlappedRefs ++ overlapped
        )
      )

      bodyComplexity <- evalExpr(func.body).map(_ + func.args.size * 5)
      evaluatedCtx   <- get[Id, EstimatorContext, EstimationError]
      overlappedChanges = overlapped.map { case ref @ (name, _) => evaluatedCtx.letDefs.get(name).map((name, _)).getOrElse(ref) }
      _ <- update(ec =>
        ec.copy(
          letDefs = ec.letDefs -- ctxArgs.keys ++ overlapped,
          overlappedRefs = ec.overlappedRefs ++ overlappedChanges
        )
      )
    } yield bodyComplexity + argsComplexity

  private def evalFuncArgs(args: List[EXPR]): EvalM[Long] =
    args.traverse(evalExpr).map(_.sum)

  private def update(f: EstimatorContext => EstimatorContext): EvalM[Unit] =
    modify[Id, EstimatorContext, EstimationError](f)

  private def const(l: Long): EvalM[Long] =
    Monad[EvalM].pure(l)
}
