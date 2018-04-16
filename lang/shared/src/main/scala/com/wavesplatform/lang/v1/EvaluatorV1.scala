package com.wavesplatform.lang.v1

import cats.data.EitherT
import com.wavesplatform.lang.ScriptVersion.Versions.V1
import com.wavesplatform.lang.TypeInfo._
import com.wavesplatform.lang.v1.Terms._
import com.wavesplatform.lang.v1.ctx._
import com.wavesplatform.lang.{ExecutionError, ExprEvaluator, TrampolinedExecResult, TypeInfo}
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

object EvaluatorV1 extends ExprEvaluator {

  override type V = V1.type
  override val version: V = V1

  private def r[T: TypeInfo](ctx: Context, t: TrampolinedExecResult[Typed.EXPR]): TrampolinedExecResult[T] =
    t flatMap { (typedExpr: Typed.EXPR) =>
      (typedExpr match {
        case Typed.BLOCK(mayBeLet, inner, blockTpe) =>
          mayBeLet match {
            case None => r(ctx, EitherT.pure(inner))(blockTpe.typeInfo)
            case Some(Typed.LET(newVarName, newVarBlock)) =>
              (ctx.letDefs.get(newVarName), ctx.functions.keys.map(_.name).exists(_ == newVarName)) match {
                case (Some(_), _) => EitherT.leftT[Coeval, T](s"Value '$newVarName' already defined in the scope")
                case (_, true) =>
                  EitherT.leftT[Coeval, Typed.EXPR](s"Value '$newVarName' can't be defined because function with such name is predefined")
                case (None, false) =>
                  val varBlockTpe                                                  = newVarBlock.tpe
                  val eitherTCoeval: TrampolinedExecResult[varBlockTpe.Underlying] = r(ctx, EitherT.pure(newVarBlock))(varBlockTpe.typeInfo)
                  val lz: LazyVal                                                  = LazyVal(varBlockTpe)(eitherTCoeval)
                  val updatedCtx: Context                                          = ctx.copy(letDefs = ctx.letDefs.updated(newVarName, lz))
                  r(updatedCtx, EitherT.pure(inner))(blockTpe.typeInfo)
              }
          }
        case Typed.REF(str, _) =>
          ctx.letDefs.get(str) match {
            case Some(lzy) => lzy.value
            case None      => EitherT.leftT[Coeval, T](s"A definition of '$str' is not found")
          }

        case Typed.CONST_LONG(v)       => EitherT.rightT[Coeval, String](v)
        case Typed.CONST_BYTEVECTOR(v) => EitherT.rightT[Coeval, String](v)
        case Typed.CONST_STRING(v)     => EitherT.rightT[Coeval, String](v)
        case Typed.TRUE                => EitherT.rightT[Coeval, String](true)
        case Typed.FALSE               => EitherT.rightT[Coeval, String](false)

        case Typed.IF(cond, t1, t2, tpe) =>
          r[Boolean](ctx, EitherT.pure(cond)) flatMap {
            case true  => r(ctx, EitherT.pure(t1))(tpe.typeInfo)
            case false => r(ctx, EitherT.pure(t2))(tpe.typeInfo)
          }

        case Typed.GETTER(expr, field, _) =>
          r[Obj](ctx, EitherT.pure(expr)).flatMap { (obj: Obj) =>
            val value: EitherT[Coeval, ExecutionError, Any] = obj.fields.find(_._1 == field) match {
              case Some((_, lzy)) => lzy.value.map(_.asInstanceOf[Any])
              case None           => EitherT.leftT[Coeval, Any](s"field '$field' not found")
            }
            value
          }
        case Typed.FUNCTION_CALL(header, args, _) =>
          import cats.data._
          import cats.instances.vector._
          import cats.syntax.all._
          ctx.functions.get(header) match {
            case Some(func) =>
              val argsVector = args
                .map(a =>
                  r(ctx, EitherT.pure(a))(a.tpe.typeInfo)
                    .map(_.asInstanceOf[Any]))
                .toVector
              val argsSequenced = argsVector.sequence[TrampolinedExecResult, Any]
              for {
                actualArgs <- argsSequenced
                r          <- func.eval(actualArgs.toList)
              } yield r
            case None => EitherT.leftT[Coeval, Any](s"function '$header' not found")
          }
      }).map { v =>
        val ti = typeInfo[T]
        v match {
          case x if typedExpr.tpe.typeInfo <:< ti => x.asInstanceOf[T]
          case _                                  => throw new Exception(s"Bad type: expected: ${ti} actual: ${typedExpr.tpe.typeInfo}")
        }
      }
    }

  def apply[A: TypeInfo](c: Context, expr: Typed.EXPR): Either[ExecutionError, A] = {
    def result = r[A](c, EitherT.pure(expr)).value.apply()
    Try(result) match {
      case Failure(ex)  => Left(ex.toString)
      case Success(res) => res
    }
  }
}
