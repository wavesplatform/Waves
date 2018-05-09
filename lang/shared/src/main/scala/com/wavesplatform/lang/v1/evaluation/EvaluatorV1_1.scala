package com.wavesplatform.lang.v1.evaluation

import com.wavesplatform.lang.v1.Terms.{TYPE, Typed}
import com.wavesplatform.lang.v1.Terms.Typed._
import com.wavesplatform.lang.v1.ctx.Context.Lenses._
import cats.implicits._
import com.wavesplatform.lang.{ExecutionError, ExecutionLog, TypeInfo}
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.FunctionHeader
import com.wavesplatform.lang.v1.ctx.{Context, LazyVal, Obj}

object EvaluatorV1_1 {

  import EvalM._

  private def evalBlock(let: LET, inner: EXPR, tpe: TYPE): EvalM[Any] = {
    import let.{name, value}
    for {
      _   <- writeLog(s"LET: $LET, TYPE: $tpe")
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
      _   <- writeLog(s"KEY: $key")
      ctx <- getContext
      result <- lets.get(ctx).get(key) match {
        case Some(lzy) => liftTER[Any](lzy.value.value)
        case None      => liftError[Any](s"A definition of '$key' is not found")
      }
    } yield result
  }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, tpe: TYPE) = {
    for {
      _   <- writeLog("Evaluating IF")
      ifc <- writeLog("Evaluating COND") *> evalExpr[Boolean](cond)
      result <- ifc match {
        case true  => writeLog("Evaluating IF_TRUE") *> evalExpr(ifTrue)(tpe.typeInfo)
        case false => writeLog("Evaluating IF_FALSE") *> evalExpr(ifFalse)(tpe.typeInfo)
      }
    } yield result
  }

  private def evalGetter(expr: EXPR, field: String) = {
    for {
      obj <- evalExpr[Obj](expr)
      result <- obj.fields.get(field) match {
        case Some(lzy) => liftTER[Any](lzy.value.value)
        case None      => liftError[Any](s"field '$field' not found")
      }
    } yield result
  }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): EvalM[Any] = {
    for {
      _   <- writeLog(s"FUNCTION HEADER: $header")
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

  private def evalExpr[T: TypeInfo](t: Typed.EXPR): EvalM[T] = {
    (t match {
      case Typed.BLOCK(let, inner, blockTpe)    => writeLog("Evaluating BLOCK") *> evalBlock(let, inner, blockTpe) <* writeLog("FINISHED")
      case Typed.REF(str, _)                    => writeLog("Evaluating REF") *> evalRef(str) <* writeLog("FINISHED")
      case Typed.CONST_LONG(v)                  => liftValue(v)
      case Typed.CONST_BYTEVECTOR(v)            => liftValue(v)
      case Typed.CONST_STRING(v)                => liftValue(v)
      case Typed.TRUE                           => liftValue(true)
      case Typed.FALSE                          => liftValue(false)
      case Typed.IF(cond, t1, t2, tpe)          => writeLog("Evaluating IF") *> evalIF(cond, t1, t2, tpe) <* writeLog("FINISHED")
      case Typed.GETTER(expr, field, _)         => writeLog("Evaluating GETTER") *> evalGetter(expr, field) <* writeLog("FINISHED")
      case Typed.FUNCTION_CALL(header, args, _) => writeLog("Evaluating FUNCTION_CALL") *> evalFunctionCall(header, args) <* writeLog("FINISHED")
    }).flatMap(v => {
      val ti = typeInfo[T]
      if (t.tpe.typeInfo <:< ti) liftValue(v.asInstanceOf[T])
      else liftError(s"Bad type: expected: ${ti} actual: ${t.tpe.typeInfo}")
    })

  }

  def apply[A: TypeInfo](c: Context, expr: Typed.EXPR): Either[(Context, ExecutionLog, ExecutionError), A] = {
    evalExpr[A](expr)
      .run(c)
  }
}
