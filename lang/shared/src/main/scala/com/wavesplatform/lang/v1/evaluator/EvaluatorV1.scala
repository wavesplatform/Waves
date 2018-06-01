package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, LET, _}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext.Lenses._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.{ExecutionError, ExprEvaluator, TypeInfo}

object EvaluatorV1 extends ExprEvaluator {

  import EvalM._

  override type V = V1.type
  override val version: V = V1

  private def evalBlock(let: LET, inner: EXPR): EvalM[Any] = {
    import let.{name, value}
    for {
      ctx <- getContext
      blockEvaluation = evalExpr[Any](value)
      lazyBlock       = LazyVal(blockEvaluation.ter(ctx))
      result <- updateContext(lets.modify(_)(_.updated(name, lazyBlock))) *> evalExpr[Any](inner)
    } yield result
  }

  private def evalRef(key: String) =
    getContext flatMap { ctx =>
      lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[Any](lzy.value.value)
        case None      => liftError[Any](s"A definition of '$key' not found")
      }
    }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR) =
    evalExpr[Boolean](cond) flatMap {
      case true  => evalExpr[Any](ifTrue)
      case false => evalExpr[Any](ifFalse)
    }

  private def evalGetter(expr: EXPR, field: String) =
    evalExpr[CaseObj](expr) flatMap {
      _.fields.get(field) match {
        case Some(eager) => liftValue[Any](eager.value)
        case None        => liftError[Any](s"field '$field' not found")
      }
    }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[Any] = {
    for {
      ctx <- getContext
      result <- funcs
        .get(ctx)
        .get(header)
        .fold(liftError[Any](s"function '$header' not found")) { func =>
          args
            .traverse[EvalM, Any](a => evalExpr[Any](a))
            .map(func.eval)
            .flatMap(r => liftTER[Any](r.value))
        }
    } yield result
  }

  private def evalExpr[T: TypeInfo](t: EXPR): EvalM[T] = {
    (t match {
      case BLOCK(let, inner, blockTpe)    => evalBlock(let, inner)
      case REF(str, _)                    => evalRef(str)
      case CONST_LONG(v)                  => liftValue(v)
      case CONST_BYTEVECTOR(v)            => liftValue(v)
      case CONST_STRING(v)                => liftValue(v)
      case TRUE                           => liftValue(true)
      case FALSE                          => liftValue(false)
      case IF(cond, t1, t2, tpe)          => evalIF(cond, t1, t2)
      case GETTER(expr, field, _)         => evalGetter(expr, field)
      case FUNCTION_CALL(header, args, _) => evalFunctionCall(header, args)
    }).flatMap(v => {
      val ti = typeInfo[T]
//      if (t.tpe.typeInfo <:< ti)
         liftValue(v.asInstanceOf[T])
//      else
//         liftError(s"Bad type: expected: $ti actual: ${t.tpe.typeInfo}")
    })
  }

  def apply[A: TypeInfo](c: EvaluationContext, expr: EXPR): (EvaluationContext, Either[ExecutionError, A]) = evalExpr[A](expr).run(c)

}
