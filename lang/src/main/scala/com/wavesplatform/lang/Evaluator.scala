package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success, Try}

object Evaluator {

  import scala.util.control.TailCalls.{TailRec, done, tailcall}

  type Defs = Map[String, (Type, Any)]

  case class Context(typeDefs: Map[String, CUSTOMTYPE], defs: Defs)
  object Context {
    val empty = Context(Map.empty, Map.empty)
  }

  type TypeResolutionError = String
  type ExcecutionError = String
  type ExecResult[T] = Either[ExcecutionError, T]

  private def r[T](ctx: Context, t: Typed.Expr): TailRec[ExecResult[T]] = {
    (t match {
      case Typed.Block(mayBeLet, inner, blockTpe) => tailcall {
        mayBeLet match {
          case None =>
            r[blockTpe.Underlying](ctx, inner)

          case Some(Typed.LET(newVarName, newVarBlock)) =>
            ctx.defs.get(newVarName) match {
              case Some(_) => done(Left(s"Value '$newVarName' already defined in the scope"))
              case None =>
                val varBlockTpe = newVarBlock.exprType
                  r[varBlockTpe.Underlying](ctx, newVarBlock).flatMap {
                    case Left(e) => done(Left(e))
                    case Right(newVarValue) =>
                      val updatedCtx = ctx.copy(defs = ctx.defs.updated(newVarName, (varBlockTpe, newVarValue)))
                      r[blockTpe.Underlying](updatedCtx, inner)
                  }
            }
        }
      }

      case let: Typed.LET => tailcall(r[let.exprType.Underlying](ctx, let.value))

      case Typed.REF(str, _) => done {
        ctx.defs.get(str) match {
          case Some((x, y)) => Right(y.asInstanceOf[x.Underlying])
          case None => Left(s"Definition '$str' not found")
        }
      }

      case Typed.CONST_INT(v) => done(Right(v))
      case Typed.CONST_BYTEVECTOR(v) => done(Right(v))
      case Typed.TRUE => done(Right(true))
      case Typed.FALSE => done(Right(false))
      case Typed.SUM(t1, t2) => tailcall {
        for {
          a1 <- r[Int](ctx, t1)
          a2 <- r[Int](ctx, t2)
        } yield a1.flatMap(v1 => a2.map(v2 => v1 + v2))
      }
      case Typed.GE(t1, t2) => tailcall {
        for {
          a1 <- r[Int](ctx, t1)
          a2 <- r[Int](ctx, t2)
        } yield a1.flatMap(v1 => a2.map(v2 => v1 >= v2))
      }
      case Typed.GT(t1, t2) => tailcall {
        for {
          a1 <- r[Int](ctx, t1)
          a2 <- r[Int](ctx, t2)
        } yield a1.flatMap(v1 => a2.map(v2 => v1 > v2))
      }

      case Typed.IF(cond, t1, t2, tpe) => tailcall {
        r[Boolean](ctx, cond).flatMap {
          case Right(true) => r[tpe.Underlying](ctx, t1)
          case Right(false) => r[tpe.Underlying](ctx, t2)
          case Left(err) => done(Left(err))
        }
      }
      case Typed.AND(t1, t2) => tailcall {
        r[Boolean](ctx, t1) flatMap {
          case Left(err) => done(Left(err))
          case Right(false) => done(Right(false))
          case Right(true) =>
            r[Boolean](ctx, t2) map {
              case Left(err) => Left(err)
              case Right(v) => Right(v)
            }
        }
      }

      case o@Typed.OR(t1, t2) => tailcall {
        r[Boolean](ctx, t1) flatMap {
          case Left(err) => done(Left(err))
          case Right(true) => done(Right(true))
          case Right(false) =>
            r[Boolean](ctx, t2) map {
              case Left(err) => Left(err)
              case Right(v) => Right(v)
            }
        }
      }

      case isDefined@Typed.IS_DEFINED(opt) =>
        tailcall {
          r[isDefined.exprType.Underlying](ctx, opt).map {
            case Right(x: Option[_]) => Right(x.isDefined)
            case Right(_) => Left("IS_DEFINED invoked on non-option type")
            case _ => Left("IS_DEFINED expression error")
          }
        }

      case Typed.GET(opt, tpe) =>
        r[tpe.Underlying](ctx, opt).map {
          case Right(Some(x)) => Right(x)
          case Right(_) => Left("GET(NONE)")
          case _ => Left("GET expression error")
        }

      case Typed.NONE => done(Right(None))

      case Typed.SOME(b, tpe) =>
        tailcall(r[tpe.Underlying](ctx, b).map(_.map(x => Some(x))))

      case eq@Typed.EQ(it1, it2) => tailcall {
        val tpe = eq.exprType
        for {
          i1 <- r[tpe.Underlying](ctx, it1)
          i2 <- r[tpe.Underlying](ctx, it2)
        } yield i1.flatMap(v1 => i2.map(v2 => v1 == v2))
      }

      case Typed.SIG_VERIFY(msg, sig, pk) => tailcall {
        for {
          s <- r[ByteVector](ctx, sig)
          m <- r[ByteVector](ctx, msg)
          p <- r[ByteVector](ctx, pk)
        } yield s.flatMap(ss => m.flatMap(mm => p.map(pp => Curve25519.verify(ss.toArray, mm.toArray, pp.toArray))))
      }

      case Typed.GETTER(expr, field, _) => tailcall {
        r[OBJECT](ctx, expr).map {
          case Right(obj) => obj.fields.find(_._1 == field) match {
            case Some((_, lzy)) => Right(lzy.value())
            case None => Left("field not found")
          }
          case Left(err) => Left(err)
        }
      }
    }).map(x => x.map(_.asInstanceOf[T]))
  }

  def apply[A](c: Context, expr: Untyped.Expr): ExecResult[A] = {
    val tr = TypeChecker(
      TypeChecker.Context(
        predefTypes = c.typeDefs,
        varDefs = c.defs.map { case (k, (t, v)) => k -> t }
      ),
      expr
    )

    tr.flatMap { expr =>
      val result = r[A](c, expr).result
      Try(result) match {
        case Failure(ex) => Left(ex.toString)
        case Success(res) => res
      }
    }
  }
}
