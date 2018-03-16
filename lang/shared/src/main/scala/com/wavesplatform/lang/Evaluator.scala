package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Terms._
import com.wavesplatform.lang.ctx._
import monix.eval.Coeval

import scala.util.{Failure, Success, Try}

object Evaluator {

  private def r[T](ctx: Context, t: TrampolinedExecResult[Typed.EXPR]): TrampolinedExecResult[T] =
    t flatMap { (typedExpr: Typed.EXPR) =>
      (typedExpr match {
        case Typed.BLOCK(mayBeLet, inner, blockTpe) =>
          mayBeLet match {
            case None => r[blockTpe.Underlying](ctx, EitherT.pure(inner))
            case Some(Typed.LET(newVarName, newVarBlock)) =>
              ctx.letDefs.get(newVarName) match {
                case Some(_) => EitherT.leftT[Coeval, T](s"Value '$newVarName' already defined in the scope")
                case None =>
                  val varBlockTpe                                                  = newVarBlock.tpe
                  val eitherTCoeval: TrampolinedExecResult[varBlockTpe.Underlying] = r[varBlockTpe.Underlying](ctx, EitherT.pure(newVarBlock))
                  val lz: LazyVal                                                  = LazyVal(varBlockTpe)(eitherTCoeval)
                  val updatedCtx: Context                                          = ctx.copy(letDefs = ctx.letDefs.updated(newVarName, lz))
                  r[blockTpe.Underlying](updatedCtx, EitherT.pure(inner))
              }
          }
        case Typed.REF(str, _) =>
          ctx.letDefs.get(str) match {
            case Some(lzy) => lzy.value
            case None      => EitherT.leftT[Coeval, T](s"A definition of '$str' is not found")
          }

        case Typed.CONST_LONG(v)        => EitherT.rightT[Coeval, String](v)
        case Typed.CONST_BYTEVECTOR(v) => EitherT.rightT[Coeval, String](v)
        case Typed.TRUE                => EitherT.rightT[Coeval, String](true)
        case Typed.FALSE               => EitherT.rightT[Coeval, String](false)

        case Typed.BINARY_OP(a, SUM_OP, b, LONG) =>
          for {
            evaluatedA <- r[Long](ctx, EitherT.pure(a))
            evaluatedB <- r[Long](ctx, EitherT.pure(b))
          } yield evaluatedA + evaluatedB

        case Typed.BINARY_OP(a, op @ (GE_OP | GT_OP), b, BOOLEAN) =>
          for {
            evaluatedA <- r[Long](ctx, EitherT.pure(a))
            evaluatedB <- r[Long](ctx, EitherT.pure(b))
          } yield
            op match {
              case GE_OP => evaluatedA >= evaluatedB
              case GT_OP => evaluatedA > evaluatedB
              case x     => throw new IllegalStateException(s"$x") // supress pattern match warning
            }

        case Typed.IF(cond, t1, t2, tpe) =>
          r[Boolean](ctx, EitherT.pure(cond)) flatMap {
            case true  => r[tpe.Underlying](ctx, EitherT.pure(t1))
            case false => r[tpe.Underlying](ctx, EitherT.pure(t2))
          }

        case Typed.BINARY_OP(t1, AND_OP, t2, BOOLEAN) =>
          r[Boolean](ctx, EitherT.pure(t1)) flatMap {
            case false => EitherT.rightT[Coeval, String](false)
            case true  => r[Boolean](ctx, EitherT.pure(t2))
          }
        case Typed.BINARY_OP(t1, OR_OP, t2, BOOLEAN) =>
          r[Boolean](ctx, EitherT.pure(t1)).flatMap {
            case true  => EitherT.rightT[Coeval, String](true)
            case false => r[Boolean](ctx, EitherT.pure(t2))
          }

        case Typed.BINARY_OP(it1, EQ_OP, it2, tpe) =>
          for {
            i1 <- r[tpe.Underlying](ctx, EitherT.pure(it1))
            i2 <- r[tpe.Underlying](ctx, EitherT.pure(it2))
          } yield i1 == i2

        case Typed.GETTER(expr, field, _) =>
          r[Obj](ctx, EitherT.pure(expr)).flatMap { (obj: Obj) =>
            val value: EitherT[Coeval, ExecutionError, Any] = obj.fields.find(_._1 == field) match {
              case Some((_, lzy)) => lzy.value.map(_.asInstanceOf[Any])
              case None           => EitherT.leftT[Coeval, Any](s"field '$field' not found")
            }
            value
          }
        case Typed.FUNCTION_CALL(name, args, tpe) =>
          import cats.data._
          import cats.instances.vector._
          import cats.syntax.all._
          ctx.functions.get(name) match {
            case Some(func) =>
              val argsVector = args
                .map(a =>
                  r[a.tpe.Underlying](ctx, EitherT.pure(a))
                    .map(_.asInstanceOf[Any]))
                .toVector
              val argsSequenced = argsVector.sequence[TrampolinedExecResult, Any]
              for {
                actualArgs <- argsSequenced
                r          <- func.eval(actualArgs.toList)
              } yield r
            case None => EitherT.leftT[Coeval, Any](s"function '$name' not found")
          }
        case Typed.BINARY_OP(_, SUM_OP, _, tpe) if tpe != LONG             => EitherT.leftT[Coeval, Any](s"Expected LONG, but got $tpe: $t")
        case Typed.BINARY_OP(_, GE_OP | GT_OP, _, tpe) if tpe != BOOLEAN  => EitherT.leftT[Coeval, Any](s"Expected LONG, but got $tpe: $t")
        case Typed.BINARY_OP(_, AND_OP | OR_OP, _, tpe) if tpe != BOOLEAN => EitherT.leftT[Coeval, Any](s"Expected BOOLEAN, but got $tpe: $t")

      }).map(_.asInstanceOf[T])
    }

  def apply[A](c: Context, expr: Typed.EXPR): Either[ExecutionError, A] = {
    def result = r[A](c, EitherT.pure(expr)).value.apply()
    Try(result) match {
      case Failure(ex)  => Left(ex.toString)
      case Success(res) => res
    }
  }
}
