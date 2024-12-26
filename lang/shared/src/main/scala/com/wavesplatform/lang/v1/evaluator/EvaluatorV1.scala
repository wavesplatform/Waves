package com.wavesplatform.lang.v1.evaluator

import cats.data.EitherT
import cats.implicits.*
import cats.{Eval, Id, Monad, StackSafeMonad}
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.*
import com.wavesplatform.lang.v1.compiler.Types.{CASETYPEREF, NOTHING}
import com.wavesplatform.lang.v1.evaluator.ContextfulNativeFunction.{Extended, Simple}
import com.wavesplatform.lang.v1.evaluator.ctx.*
import com.wavesplatform.lang.v1.task.imports.*
import com.wavesplatform.lang.v1.traits.Environment
import com.wavesplatform.lang.*

import scala.collection.mutable.ListBuffer
import scala.util.Try

object EvaluatorV1 {
  implicit val idEvalFMonad: Monad[EvalF[Id, *]] = new StackSafeMonad[EvalF[Id, *]] {
    override def flatMap[A, B](fa: Eval[A])(f: A => Eval[B]): Eval[B] =
      fa.flatMap(f).memoize

    override def pure[A](x: A): Eval[A] =
      Eval.now(x)
  }

  private val evaluator                     = new EvaluatorV1[Id, Environment]
  def apply(): EvaluatorV1[Id, Environment] = evaluator
}

class EvaluatorV1[F[_]: Monad, C[_[_]]](implicit ev: Monad[EvalF[F, *]], ev2: Monad[CoevalF[F, *]]) {

  private def evalLetBlock(let: LET, inner: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    for {
      ctx <- get[F, EnabledLogEvaluationContext[C, F], ExecutionError]
      blockEvaluation = evalExpr(let.value)
      lazyBlock       = LazyVal(blockEvaluation.ter(ctx), ctx.l(let.name))
      result <- local {
        modify[F, EnabledLogEvaluationContext[C, F], ExecutionError](ctx1 =>
          ctx1.copy(ec = ctx1.ec.copy(letDefs = ctx1.ec.letDefs.updated(let.name, lazyBlock)))
        )
          .flatMap(_ => evalExprWithCtx(inner))
      }
    } yield result

  private def evalFuncBlock(func: FUNC, inner: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] = {
    val funcHeader = FunctionHeader.User(func.name)
    val function = UserFunction(func.name, 0, NOTHING, func.args.map(n => (n, NOTHING))*)(func.body)
      .asInstanceOf[UserFunction[C]]
    local {
      modify[F, EnabledLogEvaluationContext[C, F], ExecutionError](ctx =>
        ctx.copy(ec = ctx.ec.copy(functions = ctx.ec.functions.updated(funcHeader, function)))
      )
        .flatMap(_ => evalExprWithCtx(inner))
    }
  }

  private def evalRef(key: String): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    for {
      ctx <- get[F, EnabledLogEvaluationContext[C, F], ExecutionError]
      r <- ctx.ec.letDefs.get(key) match {
        case Some(lzy) => liftTER[F, C, EVALUATED](lzy.value)
        case None      => raiseError[F, EnabledLogEvaluationContext[C, F], ExecutionError, EVALUATED](s"A definition of '$key' not found")
      }
    } yield (ctx.ec, r)

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    evalExpr(cond) flatMap {
      case TRUE  => evalExprWithCtx(ifTrue)
      case FALSE => evalExprWithCtx(ifFalse)
      case _     => ???
    }

  private def evalGetter(expr: EXPR, field: String): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] = {
    Monad[EvalM[F, C, *]].flatMap(evalExprWithCtx(expr)) { case (ctx, exprResult) =>
      val fields = exprResult.asInstanceOf[CaseObj].fields
      fields.get(field) match {
        case Some(f) => (ctx, f).pure[EvalM[F, C, *]]
        case None    => raiseError(s"A definition of '$field' not found amongst ${fields.keys}")
      }
    }
  }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    for {
      ctx <- get[F, EnabledLogEvaluationContext[C, F], ExecutionError]
      result <- ctx.ec.functions
        .get(header)
        .map {
          case func: UserFunction[C] =>
            Monad[EvalM[F, C, *]].flatMap(args.traverse(evalExpr)) { args =>
              val letDefsWithArgs = args.zip(func.signature.args).foldLeft(ctx.ec.letDefs) { case (r, (argValue, (argName, _))) =>
                r + (argName -> LazyVal.fromEvaluated(argValue, ctx.l(s"$argName")))
              }
              local {
                val newState: EvalM[F, C, Unit] =
                  set[F, EnabledLogEvaluationContext[C, F], ExecutionError](ctx.copy(ec = ctx.ec.copy(letDefs = letDefsWithArgs))).map(_.pure[F])
                Monad[EvalM[F, C, *]].flatMap(newState)(_ => evalExpr(func.ev(ctx.ec.environment, args)))
              }
            }: EvalM[F, C, EVALUATED]
          case func: NativeFunction[C] =>
            Monad[EvalM[F, C, *]].flatMap(args.traverse(evalExpr)) { args =>
              val evaluated = func.ev match {
                case f: Simple[C] =>
                  val r = Try(f.evaluate(ctx.ec.environment, args)).toEither
                    .bimap(e => CommonError(e.toString): ExecutionError, EitherT(_))
                    .pure[F]
                  EitherT(r).flatten.value.pure[Eval]
                case f: Extended[C] =>
                  f.evaluate(ctx.ec.environment, args, Int.MaxValue)
                    .map(_.map(_._1.map(_._1)))
                    .to[Eval]
              }
              liftTER[F, C, EVALUATED](evaluated)
            }
        }
        .orElse(
          // no such function, try data constructor
          header match {
            case FunctionHeader.User(typeName, _) =>
              ctx.ec.typeDefs.get(typeName).collect { case t @ CASETYPEREF(_, fields, _) =>
                args
                  .traverse[EvalM[F, C, *], EVALUATED](evalExpr)
                  .map(values => CaseObj(t, fields.map(_._1).zip(values).toMap): EVALUATED)
              }
            case _ => None
          }
        )
        .getOrElse(raiseError[F, EnabledLogEvaluationContext[C, F], ExecutionError, EVALUATED](s"function '$header' not found"))
    } yield (ctx.ec, result)

  private def evalExprWithCtx(t: EXPR): EvalM[F, C, (EvaluationContext[C, F], EVALUATED)] =
    t match {
      case LET_BLOCK(let, inner) => evalLetBlock(let, inner)
      case BLOCK(dec, inner) =>
        dec match {
          case l: LET        => evalLetBlock(l, inner)
          case f: FUNC       => evalFuncBlock(f, inner)
          case _: FAILED_DEC => raiseError("Attempt to evaluate failed declaration.")
        }
      case REF(str)                    => evalRef(str)
      case c: EVALUATED                => get[F, EnabledLogEvaluationContext[C, F], ExecutionError].map(ctx => (ctx.ec, c))
      case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
      case GETTER(expr, field)         => evalGetter(expr, field)
      case FUNCTION_CALL(header, args) => evalFunctionCall(header, args)
      case _: FAILED_EXPR              => raiseError("Attempt to evaluate failed expression.")
    }

  private def evalExpr(t: EXPR): EvalM[F, C, EVALUATED] =
    evalExprWithCtx(t).map(_._2)

  def applyWithLogging[A <: EVALUATED](c: EvaluationContext[C, F], expr: EXPR): F[Either[(ExecutionError, Log[F]), (A, Log[F])]] = {
    val log = ListBuffer[LogItem[F]]()
    val lec = EnabledLogEvaluationContext[C, F]((str: String) => (v: LetExecResult[F]) => log.append((str, v)), c)
    val r   = evalExpr(expr).map(_.asInstanceOf[A]).run(lec).value._2
    r.map(_.bimap((_, log.toList), (_, log.toList)))
  }

  def apply[A <: EVALUATED](c: EvaluationContext[C, F], expr: EXPR): F[Either[ExecutionError, A]] =
    applyWithLogging[A](c, expr).map(_.bimap(_._1, _._1))

  def applyWithCtx(c: EvaluationContext[C, F], expr: EXPR): F[Either[ExecutionError, (EvaluationContext[C, F], EVALUATED)]] =
    evalExprWithCtx(expr)
      .run(EnabledLogEvaluationContext(_ => _ => (), c))
      .value
      ._2
}
