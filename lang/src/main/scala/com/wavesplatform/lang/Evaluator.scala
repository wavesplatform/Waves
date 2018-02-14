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

  private def r[T](ctx: Context, t: Expr): TailRec[ExecResult[T]] = {
    (t match {
      case Block(mayBeLet, inner, blockTpe) => tailcall {
        val tpe = blockTpe.get
        mayBeLet match {
          case None =>
            r[tpe.Underlying](ctx, inner)

          case Some(LET(newVarName, newVarBlock)) =>
            ctx.defs.get(newVarName) match {
              case Some(_) => done(Left(s"Value '$newVarName' already defined in the scope"))
              case None =>
                newVarBlock.exprType match {
                  case None => done(Left(s"Unknown type"))
                  case Some(varBlockTpe) =>
                    r[varBlockTpe.Underlying](ctx, newVarBlock).flatMap {
                      case Left(e) => done(Left(e))
                      case Right(newVarValue) =>
                        val updatedCtx = ctx.copy(defs = ctx.defs + (newVarName -> (varBlockTpe, newVarValue)))
                        r[tpe.Underlying](updatedCtx, inner)
                    }
                }
            }
        }
      }

      case LET(_, v) =>
        t.exprType match {
          case None => done(Left(s"Unknown type"))
          case Some(tpe) => tailcall(r[tpe.Underlying](ctx, v))
        }

      case REF(str, _) => done {
        ctx.defs.get(str) match {
          case Some((x, y)) => Right(y.asInstanceOf[x.Underlying])
          case None => Left(s"Definition '$str' not found")
        }
      }

      case CONST_INT(v) => done(Right(v))
      case CONST_BYTEVECTOR(v) => done(Right(v))
      case TRUE => done(Right(true))
      case FALSE => done(Right(false))
      case SUM(t1, t2) => tailcall {
        for {
          a1 <- r[Int](ctx, t1)
          a2 <- r[Int](ctx, t2)
        } yield a1.flatMap(v1 => a2.map(v2 => v1 + v2))
      }
      case GE(t1, t2) => tailcall {
        for {
          a1 <- r[Int](ctx, t1)
          a2 <- r[Int](ctx, t2)
        } yield a1.flatMap(v1 => a2.map(v2 => v1 >= v2))
      }
      case GT(t1, t2) => tailcall {
        for {
          a1 <- r[Int](ctx, t1)
          a2 <- r[Int](ctx, t2)
        } yield a1.flatMap(v1 => a2.map(v2 => v1 > v2))
      }

      case IF(cond, t1, t2, tpe) => tailcall {
        val ifTpe = tpe.get
        r[Boolean](ctx, cond).flatMap {
          case Right(true) => r[ifTpe.Underlying](ctx, t1)
          case Right(false) => r[ifTpe.Underlying](ctx, t2)
          case Left(err) => done(Left(err))
        }
      }
      case AND(t1, t2) => tailcall {
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

      case o@OR(t1, t2) => tailcall {
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

      case IS_DEFINED(opt) =>
        opt.exprType match {
          case None => done(Left("Unresolved type"))
          case Some(tpe) => tailcall {
            r[tpe.Underlying](ctx, opt).map {
              case Right(x: Option[_]) => Right(x.isDefined)
              case Right(_) => Left("IS_DEFINED invoked on non-option type")
              case _ => Left("IS_DEFINED expression error")
            }
          }
        }

      case GET(opt, optType) =>
        optType match {
          case None => done(Left("Unresolved type"))
          case Some(tpe) => tailcall {
            r[tpe.Underlying](ctx, opt).map {
              case Right(Some(x)) => Right(x)
              case Right(_) => Left("GET(NONE)")
              case _ => Left("GET expression error")
            }
          }
        }

      case NONE => done(Right(None))

      case SOME(b, innerType) =>
        innerType match {
          case Some(tpe) => tailcall(r[tpe.Underlying](ctx, b).map(_.map(x => Some(x))))
          case None => done(Left("Inner: NONE"))
        }

      case eq@EQ(it1, it2) => tailcall {
        val tpe = eq.exprType.get
        for {
          i1 <- r[tpe.Underlying](ctx, it1)
          i2 <- r[tpe.Underlying](ctx, it2)
        } yield i1.flatMap(v1 => i2.map(v2 => v1 == v2))
      }

      case SIG_VERIFY(msg, sig, pk) => tailcall {
        for {
          s <- r[ByteVector](ctx, sig)
          m <- r[ByteVector](ctx, msg)
          p <- r[ByteVector](ctx, pk)
        } yield s.flatMap(ss => m.flatMap(mm => p.map(pp => Curve25519.verify(ss.toArray, mm.toArray, pp.toArray))))
      }

      case GETTER(expr, field, _) => tailcall {
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

  def apply[A](c: Context, expr: Expr): ExecResult[A] = {
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
    //    result
  }
}
