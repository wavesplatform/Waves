package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import com.wavesplatform.lang.ExprEvaluator.{LetExecResult, LetLogCallback, Log, LogItem}
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.LoggedEvaluationContext.Lenses._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.{ExecutionError, ExprEvaluator, TrampolinedExecResult}

import scala.collection.mutable.ListBuffer

object EvaluatorV1 extends ExprEvaluator {

  override type V = V1.type
  override val version: V = V1

  private def evalBlock(let: LET, inner: EXPR): EvalM[Any] =
    for {
      ctx <- get[LoggedEvaluationContext, ExecutionError]
      blockEvaluation = evalExpr(let.value)
      lazyBlock       = LazyVal(blockEvaluation.ter(ctx), ctx.l(let.name))
      result <- id {
        modify[LoggedEvaluationContext, ExecutionError](lets.modify(_)(_.updated(let.name, lazyBlock)))
          .flatMap(_ => evalExpr(inner))
      }
    } yield result

  private def evalRef(key: String) =
    get[LoggedEvaluationContext, ExecutionError] flatMap { ctx =>
      lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[Any](lzy.value.value)
        case None      => raiseError[LoggedEvaluationContext, ExecutionError, Any](s"A definition of '$key' not found")
      }
    }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR) =
    evalExpr(cond).map(_.asInstanceOf[Boolean]) flatMap {
      case true  => evalExpr(ifTrue)
      case false => evalExpr(ifFalse)
    }

  private def evalGetter(expr: EXPR, field: String) =
    evalExpr(expr).map(_.asInstanceOf[CaseObj]) map {
      _.fields(field)
    }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[Any] =
    for {
      ctx <- get[LoggedEvaluationContext, ExecutionError]
      result <- funcs
        .get(ctx)
        .get(header)
        .map {
          case func: UserFunction =>
            args
              .traverse[EvalM, Any](evalExpr)
              .flatMap { args =>
                val letDefsWithArgs = args.zip(func.signature.args).foldLeft(ctx.ec.letDefs) {
                  case (r, (argValue, (argName, _))) => r + (argName -> LazyVal(argValue.pure[TrampolinedExecResult], ctx.l("(arg)" + argName)))
                }
                id {
                  set(LoggedEvaluationContext.Lenses.lets.set(ctx)(letDefsWithArgs)).flatMap(_ => evalExpr(func.ev))
                }
              }
          case func: NativeFunction =>
            args
              .traverse[EvalM, Any] { x =>
                evalExpr(x)
              }
              .map(func.eval)
              .flatMap(r => liftTER[Any](r.value))
        }
        .orElse(
          // no such function, try data constructor
          header match {
            case FunctionHeader.User(typeName) =>
              types.get(ctx).get(typeName).collect {
                case t @ CaseType(_, fields) =>
                  args
                    .traverse[EvalM, Any](a => evalExpr(a))
                    .map(argValues => CaseObj(t.typeRef, fields.map(_._1).zip(argValues).toMap))
              }
            case _ => None
          }
        )
        .getOrElse(raiseError[LoggedEvaluationContext, ExecutionError, Any](s"function '$header' not found"))
    } yield result

  private def pureAny[A](v: A): EvalM[Any] = v.pure[EvalM].map(_.asInstanceOf[Any])

  private def evalExpr(t: EXPR): EvalM[Any] = t match {
    case BLOCK(let, inner)           => evalBlock(let, inner)
    case REF(str)                    => evalRef(str)
    case CONST_LONG(v)               => pureAny(v)
    case CONST_BYTEVECTOR(v)         => pureAny(v)
    case CONST_STRING(v)             => pureAny(v)
    case TRUE                        => pureAny(true)
    case FALSE                       => pureAny(false)
    case IF(cond, t1, t2)            => evalIF(cond, t1, t2)
    case GETTER(expr, field)         => evalGetter(expr, field)
    case FUNCTION_CALL(header, args) => evalFunctionCall(header, args)
  }

  def applywithLogging[A](c: EvaluationContext, expr: EXPR): (Log, Either[ExecutionError, A]) = {
    val log = ListBuffer[LogItem]()
    val r   = ap(c, expr, (str: String) => (v: LetExecResult) => log.append((str, v)))
    (log.toList, r)
  }

  def apply[A](c: EvaluationContext, expr: EXPR): Either[ExecutionError, A] = ap(c, expr, _ => _ => ())

  private def ap[A](c: EvaluationContext, expr: EXPR, llc: LetLogCallback): Either[ExecutionError, A] = {
    val lec = LoggedEvaluationContext(llc, c)
    evalExpr(expr)
      .map(_.asInstanceOf[A])
      .run(lec)
      .value
      ._2
  }

}
