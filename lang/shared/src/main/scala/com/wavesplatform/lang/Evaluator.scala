package com.wavesplatform.lang

import cats.data.EitherT
import com.wavesplatform.lang.Context.Obj
import com.wavesplatform.lang.Terms._
import monix.eval.Coeval
import scodec.bits.ByteVector
import scorex.crypto.signatures.{Curve25519, PublicKey, Signature}

import scala.util.{Failure, Success, Try}

object Evaluator {

  type TypeResolutionError      = String
  type ExcecutionError          = String
  type ExecResult[T]            = Either[ExcecutionError, T]
  type TrampolinedExecResult[T] = EitherT[Coeval, ExcecutionError, T]

  private def r[T](ctx: Context, t: Typed.EXPR): TrampolinedExecResult[T] =
    (t match {
      case Typed.BLOCK(mayBeLet, inner, blockTpe) =>
        mayBeLet match {
          case None => r[blockTpe.Underlying](ctx, inner)
          case Some(Typed.LET(newVarName, newVarBlock)) =>
            ctx.defs.get(newVarName) match {
              case Some(_) => EitherT.leftT[Coeval, T](s"Value '$newVarName' already defined in the scope")
              case None =>
                val varBlockTpe = newVarBlock.tpe
                for {
                  newVarValue <- r[varBlockTpe.Underlying](ctx, newVarBlock)
                  updatedCtx = ctx.copy(defs = ctx.defs.updated(newVarName, (varBlockTpe, newVarValue)))
                  res <- r[blockTpe.Underlying](updatedCtx, inner)
                } yield res
            }
        }

      case let: Typed.LET => r[let.tpe.Underlying](ctx, let.value)

      case Typed.REF(str, _) =>
        ctx.defs.get(str) match {
          case Some((x, y)) => EitherT.rightT[Coeval, String](y.asInstanceOf[x.Underlying])
          case None         => EitherT.leftT[Coeval, Any](s"A definition of '$str' is not found")
        }

      case Typed.CONST_INT(v)        => EitherT.rightT[Coeval, String](v)
      case Typed.CONST_BYTEVECTOR(v) => EitherT.rightT[Coeval, String](v)
      case Typed.TRUE                => EitherT.rightT[Coeval, String](true)
      case Typed.FALSE               => EitherT.rightT[Coeval, String](false)

      case Typed.BINARY_OP(a, SUM_OP, b, INT) =>
        for {
          evaluatedA <- r[Int](ctx, a)
          evaluatedB <- r[Int](ctx, b)
        } yield evaluatedA + evaluatedB

      case Typed.BINARY_OP(a, op @ (GE_OP | GT_OP), b, BOOLEAN) =>
        for {
          evaluatedA <- r[Int](ctx, a)
          evaluatedB <- r[Int](ctx, b)
        } yield
          op match {
            case GE_OP => evaluatedA >= evaluatedB
            case GT_OP => evaluatedA > evaluatedB
            case x     => throw new IllegalStateException(s"$x")
          }

      case Typed.IF(cond, t1, t2, tpe) =>
        r[Boolean](ctx, cond) flatMap {
          case true  => r[tpe.Underlying](ctx, t1)
          case false => r[tpe.Underlying](ctx, t2)
        }

      case Typed.BINARY_OP(t1, AND_OP, t2, BOOLEAN) =>
        r[Boolean](ctx, t1) flatMap {
          case false => EitherT.rightT[Coeval, String](false)
          case true  => r[Boolean](ctx, t2)
        }
      case Typed.BINARY_OP(t1, OR_OP, t2, BOOLEAN) =>
        r[Boolean](ctx, t1).flatMap {
          case true  => EitherT.rightT[Coeval, String](true)
          case false => r[Boolean](ctx, t2)
        }

      case Typed.BINARY_OP(it1, EQ_OP, it2, tpe) =>
        for {
          i1 <- r[tpe.Underlying](ctx, it1)
          i2 <- r[tpe.Underlying](ctx, it2)
        } yield i1 == i2

      case isDefined @ Typed.IS_DEFINED(opt) =>
        r[isDefined.tpe.Underlying](ctx, opt).flatMap {
          case x: Option[_] => EitherT.rightT[Coeval, String](x.isDefined)
          case _            => EitherT.leftT[Coeval, Boolean]("IS_DEFINED invoked on non-option type")
        }
      case Typed.GET(opt, tpe) =>
        r[tpe.Underlying](ctx, opt) flatMap {
          case Some(x) => EitherT.rightT[Coeval, String](x)
          case _       => EitherT.leftT[Coeval, Any]("GET(NONE)")
        }
      case Typed.NONE         => EitherT.rightT[Coeval, String](None)
      case Typed.SOME(b, tpe) => r[tpe.Underlying](ctx, b).map(Some(_))
      case Typed.SIG_VERIFY(msg, sig, pk) =>
        for {
          s <- r[ByteVector](ctx, sig)
          m <- r[ByteVector](ctx, msg)
          p <- r[ByteVector](ctx, pk)
        } yield Curve25519.verify(Signature(s.toArray), m.toArray, PublicKey(p.toArray))

      case Typed.GETTER(expr, field, _) =>
        r[Obj](ctx, expr).map { obj =>
          obj.fields.find(_._1 == field) match {
            case Some((_, lzy)) => Right(lzy.value())
            case None           => Left("field not found")
          }
        }
      case Typed.BINARY_OP(_, SUM_OP, _, tpe) if tpe != INT             => throw new IllegalArgumentException(s"Expected INT, but got $tpe: $t")
      case Typed.BINARY_OP(_, GE_OP | GT_OP, _, tpe) if tpe != BOOLEAN  => throw new IllegalArgumentException(s"Expected INT, but got $tpe: $t")
      case Typed.BINARY_OP(_, AND_OP | OR_OP, _, tpe) if tpe != BOOLEAN => throw new IllegalArgumentException(s"Expected BOOLEAN, but got $tpe: $t")

    }).map(_.asInstanceOf[T])

  def apply[A](c: Context, expr: Typed.EXPR): ExecResult[A] = {
    def result = r[A](c, expr).value.apply()

//    Try(result) match {
//      case Failure(ex)  => Left(ex.toString)
//      case Success(res) => res
//    }
    result

  }
}
