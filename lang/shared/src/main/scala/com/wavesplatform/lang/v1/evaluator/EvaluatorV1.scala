package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms._
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext.Lenses._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.{ExecutionError, ExprEvaluator}

object EvaluatorV1 extends ExprEvaluator {

  import EvalM._

  override type V = V1.type
  override val version: V = V1

  private def evalBlock(let: LET, inner: EXPR): EvalM[Any] =
    for {
      ctx <- getContext
      blockEvaluation = evalExpr(let.value)
      lazyBlock       = LazyVal(blockEvaluation.ter(ctx))
      result <- updateContext(lets.modify(_)(_.updated(let.name, lazyBlock))) *> evalExpr(inner)
    } yield result

  private def evalRef(key: String) =
    getContext flatMap { ctx =>
      lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[Any](lzy.value.value)
        case None      => liftError[Any](s"A definition of '$key' not found")
      }
    }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR) =
    evalExpr(cond).map(_.asInstanceOf[Boolean]) flatMap {
      case true  => evalExpr(ifTrue)
      case false => evalExpr(ifFalse)
    }

  private def evalGetter(expr: EXPR, field: String) =
    evalExpr(expr).map(_.asInstanceOf[CaseObj]) flatMap {
      _.fields.get(field) match {
        case Some(eager) => liftValue[Any](eager.value)
        case None        => liftError[Any](s"field '$field' not found")
      }
    }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[Any] =
    for {
      ctx <- getContext
      result <- funcs
        .get(ctx)
        .get(header)
        .fold(liftError[Any](s"function '$header' not found")) { func =>
          args
            .traverse[EvalM, Any](a => evalExpr(a))
            .map(func.eval)
            .flatMap(r => liftTER[Any](r.value))
        }
    } yield result

  private def evalExpr(t: EXPR): EvalM[Any] = t match {
    case BLOCK(let, inner, _)           => evalBlock(let, inner)
    case REF(str, _)                    => evalRef(str)
    case CONST_LONG(v)                  => liftValue(v)
    case CONST_BYTEVECTOR(v)            => liftValue(v)
    case CONST_STRING(v)                => liftValue(v)
    case TRUE                           => liftValue(true)
    case FALSE                          => liftValue(false)
    case IF(cond, t1, t2, _)            => evalIF(cond, t1, t2)
    case GETTER(expr, field, _)         => evalGetter(expr, field)
    case FUNCTION_CALL(header, args, _) => evalFunctionCall(header, args)
  }

  def apply[A](c: EvaluationContext, expr: EXPR): (EvaluationContext, Either[ExecutionError, A]) = evalExpr(expr).map(_.asInstanceOf[A]).run(c)

}
