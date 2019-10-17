package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import cats.{Eval, Id, Monad, StackSafeMonad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.compiler.Types.CASETYPEREF
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext.Lenses
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.{EvalF, ExecutionError}

import scala.collection.mutable.ListBuffer

object EvaluatorV1 {
  implicit val idEvalFMonad: Monad[EvalF[Id, ?]] = new StackSafeMonad[EvalF[Id, ?]] {
    override def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] =
      fa.flatMap(f).memoize

    override def pure[A](x: A): Eval[A] =
      Eval.now(x)
  }

  private val evaluator = new EvaluatorV1[Id, Environment]
  def apply(): EvaluatorV1[Id, Environment] = evaluator
}

class EvaluatorV1[F[_] : Monad, C[_[_]]](implicit ev: Monad[EvalF[F, ?]]) {
  private val lenses = new Lenses[F, C]
  import lenses._

  private def evalLetBlock(let: LET, inner: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    for {
      ctx <- get[F, LoggedEvaluationContext[C, F], ExecutionError]
      blockEvaluation = evalExpr(let.value)
      lazyBlock       = LazyVal(blockEvaluation.ter(ctx), ctx.l(let.name))
      result <- local {
        modify[F, LoggedEvaluationContext[C, F], ExecutionError](lets.modify(_)(_.updated(let.name, lazyBlock)))
          .flatMap(_ => evalExprWithCtx(inner))
      }
    } yield result

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] = {
    val funcHeader = FunctionHeader.User(func.name)
    val function = UserFunction(func.name, 0, null, func.args.map(n => (n, null)): _*)(func.body)
        .asInstanceOf[UserFunction[C]]
    local {
      modify[F, LoggedEvaluationContext[C, F], ExecutionError](funcs.modify(_)(_.updated(funcHeader, function)))
        .flatMap(_ => evalExprWithCtx(inner))
    }
  }

  private def evalRef(key: String): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    for {
      ctx <- get[F, LoggedEvaluationContext[C, F], ExecutionError]
      r   <- lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[F, C, EVALUATED](lzy.value)
        case None      => raiseError[F, LoggedEvaluationContext[C, F], ExecutionError, EVALUATED](s"A definition of '$key' not found")
      }
    } yield (ctx.ec, r)

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    evalExpr(cond) flatMap {
      case TRUE  => evalExprWithCtx(ifTrue)
      case FALSE => evalExprWithCtx(ifFalse)
      case _     => ???
    }

  private def evalGetter(expr: EXPR, field: String): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] = {
    Monad[EvalM[F, C, ?]].flatMap(evalExprWithCtx(expr)) { case (ctx, exprResult) =>
      val fields = exprResult.asInstanceOf[CaseObj].fields
      fields.get(field) match {
        case Some(f) => (ctx, f).pure[EvalM[F, C, ?]]
        case None    => raiseError(s"A definition of '$field' not found amongst ${fields.keys}")
      }
    }
  }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    for {
      ctx <- get[F, LoggedEvaluationContext[C, F], ExecutionError]
      result <- funcs
        .get(ctx)
        .get(header)
        .map {
          case func: UserFunction[C] =>
            Monad[EvalM[F, C, ?]].flatMap(args.traverse(evalExpr)) { args =>
              val letDefsWithArgs = args.zip(func.signature.args).foldLeft(ctx.ec.letDefs) {
                case (r, (argValue, (argName, _))) =>
                  r + (argName -> LazyVal.fromEvaluated(argValue, ctx.l(s"$argName")))
              }
              local {
                val newState: EvalM[F, C, Unit] = set[F, LoggedEvaluationContext[C, F], ExecutionError](lets.set(ctx)(letDefsWithArgs)).map(_.pure[F])
                Monad[EvalM[F, C, ?]].flatMap(newState)(_ => evalExpr(func.ev(ctx.ec.environment)))
              }
            }: EvalM[F, C, EVALUATED]
          case func: NativeFunction[C] =>
            Monad[EvalM[F, C, ?]].flatMap(args.traverse(evalExpr))(args =>
              liftTER[F, C, EVALUATED](func.eval[F](ctx.ec.environment, args).value)
            )
        }
        .orElse(
          // no such function, try data constructor
          header match {
            case FunctionHeader.User(typeName, _) =>
              types.get(ctx).get(typeName).collect {
                case t @ CASETYPEREF(_, fields) =>
                  args
                    .traverse[EvalM[F, C, ?], EVALUATED](evalExpr)
                    .map(values => CaseObj(t, fields.map(_._1).zip(values).toMap): EVALUATED)
              }
            case _ => None
          }
        )
        .getOrElse(raiseError[F, LoggedEvaluationContext[C, F], ExecutionError, EVALUATED](s"function '$header' not found"))
    } yield (ctx.ec, result)

  private def evalExprWithCtx(t: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    t match {
      case LET_BLOCK(let, inner) => evalLetBlock(let, inner)
      case BLOCK(dec, inner) =>
        dec match {
          case l: LET  => evalLetBlock(l, inner)
          case f: FUNC => evalFuncBlock(f, inner)
        }
      case REF(str)                    => evalRef(str)
      case c: EVALUATED                => get[F, LoggedEvaluationContext[C, F], ExecutionError].map(ctx => (ctx.ec, c))
      case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
      case GETTER(expr, field)         => evalGetter(expr, field)
      case FUNCTION_CALL(header, args) => evalFunctionCall(header, args)
    }

  def evalExpr(t: EXPR): EvalM[F, C, EVALUATED] =
    evalExprWithCtx(t).map(_._2)

  def applyWithLogging[A <: EVALUATED](c: EvaluationContext[C, F], expr: EXPR): (Log[F], F[Either[ExecutionError, A]]) = {
    val log = ListBuffer[LogItem[F]]()
    val r   = ap[A](c, expr, (str: String) => (v: LetExecResult[F]) => log.append((str, v)))
    (log.toList, r)
  }

  def applyWithLogging[A <: EVALUATED](
    c:    Either[ExecutionError, EvaluationContext[C, F]],
    expr: EXPR
  ): (Log[F], F[Either[ExecutionError, A]]) = {
    val log = ListBuffer[LogItem[F]]()
    val r = c.flatTraverse(ap[A](_, expr, str => v => log.append((str, v))))
    (log.toList, r)
  }

  def apply[A <: EVALUATED](c: EvaluationContext[C, F], expr: EXPR): F[Either[ExecutionError, A]] =
    ap(c, expr, _ => _ => ())

  def evalWithLogging(c: EvaluationContext[C, F], evalC: EvalM[F, C, EVALUATED]): (Log[F], F[Either[ExecutionError, EVALUATED]]) = {
    val log = ListBuffer[LogItem[F]]()
    val llc = (str: String) => (v: LetExecResult[F]) => log.append((str, v))
    val lec = LoggedEvaluationContext(llc, c)
    val res = evalC.run(lec).value._2
    (log.toList, res)
  }

  def evalWithLogging(
    ctx:   Either[ExecutionError, EvaluationContext[C, F]],
    evalC: EvalM[F, C, EVALUATED]
  ): (Log[F], F[Either[ExecutionError, EVALUATED]]) = {
    val log = ListBuffer[LogItem[F]]()
    val llc = (str: String) => (v: LetExecResult[F]) => log.append((str, v))
    val res = ctx
      .map(LoggedEvaluationContext(llc, _))
      .flatTraverse(evalC.run(_).value._2)

    (log.toList, res)
  }

  private def ap[A <: EVALUATED](
    c: EvaluationContext[C, F],
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

  def applyWithCtx(c: EvaluationContext[C, F], expr: EXPR): F[Either[ExecutionError, (EvaluationContext[C, F], EVALUATED)]] =
    evalExprWithCtx(expr)
      .run(LoggedEvaluationContext(_ => _ => (), c))
      .value
      ._2
}
