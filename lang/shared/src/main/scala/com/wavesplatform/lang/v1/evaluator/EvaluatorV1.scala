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

  private def evalBlock(let: LET, inner: EXPR, tpe: TYPE): EvalM[Any] = {
    import let.{name, value}
    for {
      ctx <- getContext
      result <- {
        if (lets.get(ctx).get(name).isDefined)
          liftError(s"Value '$name' already defined in the scope")
        else if (funcs.get(ctx).keys.exists(_.name == name))
          liftError(s"Value '$name' can't be defined because function with such name is predefined")
        else {
          val blockEvaluation = evalExpr(value)(value.tpe.typeInfo)
          val lazyBlock       = LazyVal(value.tpe)(blockEvaluation.ter(ctx))
          updateContext(lets.modify(_)(_.updated(name, lazyBlock))) *> evalExpr(inner)(tpe.typeInfo)
        }
      }
    } yield result
  }

  private def evalRef(key: String) = {
    for {
      ctx <- getContext
      result <- lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[Any](lzy.value.value)
        case None      => liftError[Any](s"A definition of '$key' is not found")
      }
    } yield result
  }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, tpe: TYPE) = {
    for {
      ifc <- evalExpr[Boolean](cond)
      result <- ifc match {
        case true  => evalExpr(ifTrue)(tpe.typeInfo)
        case false => evalExpr(ifFalse)(tpe.typeInfo)
      }
    } yield result
  }

  private def evalGetter(expr: EXPR, field: String) = {
    for {
      obj <- evalExpr[CaseObj](expr)
      result <- obj match {
        case CaseObj(_, fields) =>
          fields.get(field) match {
            case Some(eager) => liftValue[Any](eager.value)
            case None        => liftError[Any](s"field '$field' not found")
          }
      }

    } yield result
  }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[Any] = {
    for {
      ctx <- getContext
      result <- funcs
        .get(ctx)
        .get(header)
        .fold(liftError[Any](s"function '$header' not found")) { func =>
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
      case CONST_LONG(v)                  => liftValue(v)
      case CONST_BYTEVECTOR(v)            => liftValue(v)
      case CONST_STRING(v)                => liftValue(v)
      case TRUE                           => liftValue(true)
      case FALSE                          => liftValue(false)
      case IF(cond, t1, t2, tpe)          => evalIF(cond, t1, t2, tpe)
      case GETTER(expr, field, _)         => evalGetter(expr, field)
      case FUNCTION_CALL(header, args, _) => evalFunctionCall(header, args)
    }).flatMap(v => {
      val ti = typeInfo[T]
      if (t.tpe.typeInfo <:< ti) liftValue(v.asInstanceOf[T])
      else liftError(s"Bad type: expected: $ti actual: ${t.tpe.typeInfo}")
    })

  }

  def apply[A: TypeInfo](c: EvaluationContext, expr: EXPR): Either[(EvaluationContext, ExecutionError), A] = {
    evalExpr[A](expr).run(c)
  }
}
