package com.wavesplatform.lang.v1

import cats.data.{EitherT, StateT}
import cats.implicits._
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.parser.Terms.Typed.{EXPR, LET}
import com.wavesplatform.lang.v1.parser.Terms._
import com.wavesplatform.lang.v1.ctx._
import com.wavesplatform.lang._
import monix.eval.Coeval
import EvaluationContext._

object EvaluatorV1 extends ExprEvaluator {

  override type V = V1.type
  override val version: V = V1

  private type F0[A] = StateT[Coeval, EvaluationContext, A]
  private type FF[A] = EitherT[F0, ExecutionError, A]

  private def getContext: FF[EvaluationContext] =
    EitherT.apply[F0, ExecutionError, EvaluationContext](StateT.get[Coeval, EvaluationContext].map(_.asRight[ExecutionError]))
  private def updateContext(f: EvaluationContext => EvaluationContext): FF[Unit] =
    EitherT.apply[F0, ExecutionError, Unit](StateT.modify[Coeval, EvaluationContext](f).map(_.asRight[ExecutionError]))

  private def writeLog(l: String): FF[Unit] = updateContext(_.logAppend(l))

  private def liftR[A](x: A): FF[A]                = EitherT.apply[F0, ExecutionError, A](StateT(s => Coeval.evalOnce((s, Right(x)))))
  private def liftL[A](err: ExecutionError): FF[A] = EitherT.apply[F0, ExecutionError, A](StateT(s => Coeval.evalOnce((s, Left(err)))))

  private def liftCE[A](ei: Coeval[ExecutionError Either A]): FF[A] = EitherT.apply[F0, ExecutionError, A](StateT(s => ei.map(v => (s, v))))

  private def toTER[A](ctx: EvaluationContext, fa: FF[A]): TrampolinedExecResult[A] = {
    fa.value
      .run(ctx)
      .map(t => EitherT.fromEither[Coeval](t._2))
      .value
  }

  private def evalBlock(let: LET, inner: EXPR, tpe: TYPE): FF[Any] = {
    import let.{name, value}
    for {
      _   <- writeLog(s"LET: $let; TYPE: $tpe")
      ctx <- getContext
      result <- {
        if (lets.get(ctx).get(name).isDefined) liftL(s"Value '$name' already defined in the scope")
        else if (funcs.get(ctx).keys.exists(_.name == name))
          liftL(s"Value '$name' can't be defined because function with such name is predefined")
        else {
          val blockEvaluation = evalExpr(value)(value.tpe.typeInfo)
          val lazyBlock       = LazyVal(value.tpe)(toTER(ctx, blockEvaluation))
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
        case Some(lzy) => liftCE[Any](lzy.value.value)
        case None      => liftL[Any](s"A definition of '$key' is not found")
      }
    } yield result
  }

  private def evalIF(cond: EXPR, ifTrue: EXPR, ifFalse: EXPR, tpe: TYPE) = {
    for {
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
        case Some(lzy) => liftCE[Any](lzy.value.value)
        case None      => liftL[Any](s"field '$field' not found")
      }
    } yield result
  }

  private def evalFunctionCall(header: FunctionHeader, args: List[EXPR]): FF[Any] = {
    for {
      _   <- writeLog(s"FUNCTION HEADER: $header")
      ctx <- getContext
      result <- funcs
        .get(ctx)
        .get(header)
        .fold(liftL[Any](s"function '$header' not found")) { func =>
          args
            .traverse[FF, Any](a => evalExpr(a)(a.tpe.typeInfo).map(_.asInstanceOf[Any]))
            .map(func.eval)
            .flatMap(r => liftCE[Any](r.value))
        }
    } yield result
  }

  private def evalExpr[T: TypeInfo](t: Typed.EXPR): FF[T] = {
    (t match {
      case Typed.BLOCK(let, inner, blockTpe) =>
        writeLog("Evaluating BLOCK") *> evalBlock(let, inner, blockTpe) <* writeLog("FINISHED")
      case Typed.REF(str, _) =>
        writeLog("Evaluating REF") *> evalRef(str) <* writeLog("FINISHED")
      case Typed.CONST_LONG(v)       => liftR(v)
      case Typed.CONST_BYTEVECTOR(v) => liftR(v)
      case Typed.CONST_STRING(v)     => liftR(v)
      case Typed.TRUE                => liftR(true)
      case Typed.FALSE               => liftR(false)
      case Typed.IF(cond, t1, t2, tpe) =>
        writeLog("Evaluating IF") *> evalIF(cond, t1, t2, tpe) <* writeLog("FINISHED")
      case Typed.GETTER(expr, field, _) =>
        writeLog(s"Evaluating GETTER") *> evalGetter(expr, field) <* writeLog("FINISHED")
      case Typed.FUNCTION_CALL(header, args, _) =>
        writeLog(s"Evaluating FUNCTION_CALL") *> evalFunctionCall(header, args) <* writeLog("FINISHED")
    }).flatMap(v => {
      val ti = typeInfo[T]
      if (t.tpe.typeInfo <:< ti) liftR(v.asInstanceOf[T])
      else liftL(s"Bad type: expected: ${ti} actual: ${t.tpe.typeInfo}")
    })

  }

  def apply[A: TypeInfo](c: Context, expr: Typed.EXPR): Either[(Context, ExecutionLog, ExecutionError), A] = {
    def evaluation = evalExpr[A](expr).value.run(EvaluationContext(c))

    evaluation
      .map({
        case (_, Right(v))    => Right(v)
        case (ctx, Left(err)) => Left((ctx.context, ctx.getLog, err))
      })
      .apply()
  }
}
