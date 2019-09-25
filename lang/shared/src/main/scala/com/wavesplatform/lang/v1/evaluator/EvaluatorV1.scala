package com.wavesplatform.lang.v1.evaluator

import cats.data.EitherT
import cats.implicits._
import cats.{Eval, Id, Monad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext.Lenses
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.ExecutionError

import scala.collection.mutable.ListBuffer

object EvaluatorV1 {
  private def evaluator = new EvaluatorV1[Id]
  def apply(): EvaluatorV1[Id] = evaluator
}

class EvaluatorV1[F[_] : Monad] {
  private val lenses = new Lenses[F]
  import lenses._

  private def evalLetBlock(let: LET, inner: EXPR): EvalM[F, (EvaluationContext[F], EVALUATED)] =
    for {
      ctx <- get[F, LoggedEvaluationContext[F], ExecutionError]
      blockEvaluation = evalExpr(let.value)
      lazyBlock       = LazyVal(blockEvaluation.ter(ctx), ctx.l(let.name))
      result <- local {
        modify[F, LoggedEvaluationContext[F], ExecutionError](lets.modify(_)(_.updated(let.name, lazyBlock)))
          .flatMap(_ => evalExprWithCtx(inner))
      }
    } yield result

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[F, (EvaluationContext[F], EVALUATED)] = {
    val funcHeader = FunctionHeader.User(func.name)
    val function = UserFunction[F](func.name, 0, null, func.args.map(n => (n, null)): _*)(func.body)
    local {
      modify[F, LoggedEvaluationContext[F], ExecutionError](funcs.modify(_)(_.updated(funcHeader, function)))
        .flatMap(_ => evalExprWithCtx(inner))
    }
  }

  private def evalRef(key: String): EvalM[F, (EvaluationContext[F], EVALUATED)] =
    for {
      ctx <- get[F, LoggedEvaluationContext[F], ExecutionError]
      r   <- lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[F, EVALUATED](lzy.value)
        case None      => raiseError[F, LoggedEvaluationContext[F], ExecutionError, EVALUATED](s"A definition of '$key' not found")
      }
    } yield (ctx.ec, r)

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[F, (EvaluationContext[F], EVALUATED)] =
    evalExpr(cond) flatMap {
      case TRUE  => evalExprWithCtx(ifTrue)
      case FALSE => evalExprWithCtx(ifFalse)
      case _     => ???
    }

  private def evalGetter(expr: EXPR, field: String): EvalM[F, (EvaluationContext[F], EVALUATED)] = {
    Monad[EvalM[F, ?]].flatMap(evalExprWithCtx(expr)) { case (ctx, exprResult) =>
      val fields = exprResult.asInstanceOf[CaseObj].fields
      fields.get(field) match {
        case Some(f) => (ctx, f).pure[EvalM[F, ?]]
        case None    => raiseError(s"A definition of '$field' not found amongst ${fields.keys}")
      }
    }
  }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[F, (EvaluationContext[F], EVALUATED)] =
    for {
      ctx <- get[F, LoggedEvaluationContext[F], ExecutionError]
      result <- funcs
        .get(ctx)
        .get(header)
        .map {
          case func: UserFunction[F] =>
            Monad[EvalM[F, ?]].flatMap(args.traverse(evalExpr)) { args =>
              val letDefsWithArgs = args.zip(func.signature.args).foldLeft(ctx.ec.letDefs) {
                case (r, (argValue, (argName, _))) =>
                  r + (argName -> LazyVal(argValue, ctx.l(s"$argName")))
              }
              local {
                val newState: EvalM[F, Unit] = set[F, LoggedEvaluationContext[F], ExecutionError](lets.set(ctx)(letDefsWithArgs)).map(_.pure[F])
                Monad[EvalM[F, ?]].flatMap(newState)(_ => evalExpr(func.ev))
              }
            }: EvalM[F, EVALUATED]
          case func: NativeFunction[F] =>
            Monad[EvalM[F, ?]].flatMap(args.traverse(evalExpr))(args =>
              liftTER[F, EVALUATED](func.eval(args).value)
            )
        }
        .orElse(
          // no such function, try data constructor
          header match {
            case FunctionHeader.User(typeName, _) =>
              types.get(ctx).get(typeName).collect {
                case t @ CASETYPEREF(_, fields) =>
                  args
                    .traverse[EvalM[F, ?], EVALUATED](evalExpr)
                    .map(values => CaseObj(t, fields.map(_._1).zip(values).toMap): EVALUATED)
              }
            case _ => None
          }
        )
        .getOrElse(raiseError[F, LoggedEvaluationContext[F], ExecutionError, EVALUATED](s"function '$header' not found"))
    } yield (ctx.ec, result)

  private def evalExprWithCtx(t: EXPR): EvalM[F, (EvaluationContext[F], EVALUATED)] =
    t match {
      case LET_BLOCK(let, inner) => evalLetBlock(let, inner)
      case BLOCK(dec, inner) =>
        dec match {
          case l: LET  => evalLetBlock(l, inner)
          case f: FUNC => evalFuncBlock(f, inner)
        }
      case REF(str)                    => evalRef(str)
      case c: EVALUATED                => get[F, LoggedEvaluationContext[F], ExecutionError].map(ctx => (ctx.ec, c))
      case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
      case GETTER(expr, field)         => evalGetter(expr, field)
      case FUNCTION_CALL(header, args) => evalFunctionCall(header, args)
    }

  def evalExpr(t: EXPR): EvalM[F, EVALUATED] =
    evalExprWithCtx(t).map(_._2)

  def applyWithLogging[A <: EVALUATED](c: EvaluationContext[F], expr: EXPR): (Log[F], F[Either[ExecutionError, A]]) = {
    val log = ListBuffer[LogItem[F]]()
    val r   = ap[A](c, expr, (str: String) => (v: LetExecResult[F]) => log.append((str, v)))
    (log.toList, r)
  }

  def applyWithLogging[A <: EVALUATED](
    c:    Either[ExecutionError, EvaluationContext[F]],
    expr: EXPR
  ): (Log[F], F[Either[ExecutionError, A]]) = {
    val log = ListBuffer[LogItem[F]]()
    val r = c.flatTraverse(ap[A](_, expr, str => v => log.append((str, v))))
    (log.toList, r)
  }

  def apply[A <: EVALUATED](c: EvaluationContext[F], expr: EXPR): F[Either[ExecutionError, A]] =
    ap(c, expr, _ => _ => ())

  def evalWithLogging(c: EvaluationContext[F], evalC: EvalM[F, EVALUATED]): (Log[F], F[Either[ExecutionError, EVALUATED]]) = {
    val log = ListBuffer[LogItem[F]]()
    val llc = (str: String) => (v: LetExecResult[F]) => log.append((str, v))
    val lec = LoggedEvaluationContext(llc, c)
    val res = evalC.run(lec).value._2
    (log.toList, res)
  }

  def evalWithLogging(
    ctx:   Either[ExecutionError, EvaluationContext[F]],
    evalC: EvalM[F, EVALUATED]
  ): (Log[F], F[Either[ExecutionError, EVALUATED]]) = {
    val log = ListBuffer[LogItem[F]]()
    val llc = (str: String) => (v: LetExecResult[F]) => log.append((str, v))
    val res = ctx
      .map(LoggedEvaluationContext(llc, _))
      .flatTraverse(evalC.run(_).value._2)

    (log.toList, res)
  }

  private def ap[A <: EVALUATED](
    c: EvaluationContext[F],
    expr: EXPR,
    llc: LetLogCallback[F]
  ): F[Either[ExecutionError, A]] = {
    val lec = LoggedEvaluationContext(llc, c)
    evalExpr(expr)
      .map(_.asInstanceOf[A])
      .run(lec)
      .value
      ._2
  }

  def applyWithCtx(c: EvaluationContext[F], expr: EXPR): F[Either[ExecutionError, (EvaluationContext[F], EVALUATED)]] =
    evalExprWithCtx(expr)
      .run(LoggedEvaluationContext(_ => _ => (), c))
      .value
      ._2
}
