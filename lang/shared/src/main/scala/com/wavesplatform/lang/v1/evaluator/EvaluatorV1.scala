package com.wavesplatform.lang.v1.evaluator

import cats.implicits._
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.compiler.Terms.{EXPR, LET, _}
import com.wavesplatform.lang.v1.evaluator.ctx.EvaluationContext.Lenses._
import com.wavesplatform.lang.v1.evaluator.ctx._
import com.wavesplatform.lang.v1.task.imports._
import com.wavesplatform.lang.{ExecutionError, ExprEvaluator, TypeInfo}


object EvaluatorV1 extends ExprEvaluator {

  override type V = V1.type
  override val version: V = V1

  private def evalBlock(let: LET, inner: EXPR, tpe: TYPE): EvalM[Any] = {
    import let.{name, value}
    for {
      ctx <- get[EvaluationContext, ExecutionError]
      blockEvaluation = evalExpr(value)(value.tpe.typeInfo)
      lazyBlock       = LazyVal(value.tpe)(blockEvaluation.ter(ctx))
      _ <- modify[EvaluationContext, ExecutionError](lets.modify(_)(_.updated(name, lazyBlock)))
      result <- evalExpr(inner)(tpe.typeInfo)
    } yield result
  }

  private def evalRef(key: String) =
    get[EvaluationContext, ExecutionError] flatMap { ctx =>
      lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[Any](lzy.value.value)
        case None      => raiseError[EvaluationContext, ExecutionError, Any](s"A definition of '$key' not found")
      }
    }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, tpe: TYPE) =
    evalExpr[Boolean](cond) flatMap {
      case true  => evalExpr(ifTrue)(tpe.typeInfo)
      case false => evalExpr(ifFalse)(tpe.typeInfo)
    }

  private def evalGetter(expr: EXPR, field: String) =
    evalExpr[CaseObj](expr) flatMap {
      _.fields.get(field) match {
        case Some(eager) => eager.value.asInstanceOf[Any].pure[EvalM]
        case None        => raiseError[EvaluationContext, ExecutionError, Any](s"field '$field' not found")
      }
    }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[Any] = {
    for {
      ctx <- get[EvaluationContext, ExecutionError]
      result <- funcs
        .get(ctx)
        .get(header)
        .fold(raiseError[EvaluationContext, ExecutionError, Any](s"function '$header' not found")) { func =>
          args
            .traverse[EvalM, Any](a => evalExpr(a)(a.tpe.typeInfo).map(_.asInstanceOf[Any]))
            .map(func.eval)
            .flatMap(r => liftTER[Any](r.value))
        }
    } yield result
  }

  private def evalExpr[T: TypeInfo](t: EXPR): EvalM[T] = {
    (t match {
      case BLOCK(let, inner, blockTpe)    => evalBlock(let, inner, blockTpe)
      case REF(str, _)                    => evalRef(str)
      case CONST_LONG(v)                  => v.pure[EvalM]
      case CONST_BYTEVECTOR(v)            => v.pure[EvalM]
      case CONST_STRING(v)                => v.pure[EvalM]
      case TRUE                           => true.pure[EvalM]
      case FALSE                          => false.pure[EvalM]
      case IF(cond, t1, t2, tpe)          => evalIF(cond, t1, t2, tpe)
      case GETTER(expr, field, _)         => evalGetter(expr, field)
      case FUNCTION_CALL(header, args, _) => evalFunctionCall(header, args)
    }).flatMap(v => {
      val ti = typeInfo[T]
      if (t.tpe.typeInfo <:< ti) v.asInstanceOf[T].pure[EvalM]
      else raiseError(s"Bad type: expected: $ti actual: ${t.tpe.typeInfo}")
    })
  }

  def apply[A: TypeInfo](c: EvaluationContext, expr: EXPR): (EvaluationContext, Either[ExecutionError, A]) = {
    evalExpr[A](expr)
      .run(c)
      .value
  }

}
