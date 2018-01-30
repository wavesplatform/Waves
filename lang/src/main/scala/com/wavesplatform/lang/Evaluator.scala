package com.wavesplatform.lang

import com.wavesplatform.lang.Terms._
import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Success, Try}

object Evaluator {
  type Defs = Map[String, (Type, Any)]

  case class Context(domain: Domain, defs: Defs)

  type TypeResolutionError = String
  type ExcecutionError     = String
  type ExecResult[T]       = Either[ExcecutionError, T]

  def resolveType(defs: Defs, t: Expr): Either[TypeResolutionError, Type] = t match {
    case REF(key) => defs.get(key).map(_._1).toRight(s"Typecheck failed: Cannot resolve type of $key")
    case CExpr(maybeLet, expr) =>
      maybeLet match {
        case Some(let) =>
          for {
            innerType <- resolveType(defs, let.value)
            result    <- resolveType(defs + (let.name -> (innerType, null)), expr)
          } yield result
        case None => resolveType(defs, expr)
      }
    case IF(_, r, l) =>
      for {
        rType <- resolveType(defs, r)
        lType <- resolveType(defs, l)
        _     <- Either.cond(rType == lType, (), s"Typecheck failed: RType($rType) differs from LType($lType)")
      } yield rType
    case get: GET =>
      resolveType(defs, get.t) flatMap {
        case OPTION(in) => Right(in)
        case x          => Left(s"Typecheck failed: GET called on $x, but only call on OPTION[_] is allowed")
      }
    case x => Right(x.predefinedType.get)
  }

  def evR[T](ctx: Context, t: Expr): ExecResult[T] =
    (t match {
      case CExpr(maybelet, inner) =>
        maybelet match {
          case None => resolveType(ctx.defs, inner).flatMap(termType => evR[termType.Underlying](ctx, inner))
          case Some(LET(newVarName: String, newVarExpr: CExpr)) =>
            for {
              newVarType  <- resolveType(ctx.defs, newVarExpr)
              newVarValue <- evR[newVarType.Underlying](ctx, newVarExpr)
              _           <- Either.cond(ctx.defs.get(newVarName).isEmpty, (), s"Value '$newVarName' already defined in the scope")
              newDefs = ctx.defs + (newVarName -> (newVarType, newVarValue))
              termType <- resolveType(newDefs, inner)
              res      <- evR[termType.Underlying](ctx.copy(defs = newDefs), inner)
            } yield res
        }
      case LET(name, v) => resolveType(ctx.defs, v).flatMap(rType => evR[rType.Underlying](ctx, v))
      case REF(str) =>
        ctx.defs.get(str) match {
          case Some((x, y)) => Right(y.asInstanceOf[x.Underlying])
          case None         => Left(s"Definition '$str' not found")
        }
      case CONST_INT(v)        => Right(v)
      case CONST_BYTEVECTOR(v) => Right(v)
      case SUM(t1, t2) =>
        for {
          a1 <- evR[Int](ctx, t1)
          a2 <- evR[Int](ctx, t2)
        } yield a1 + a2
      case GE(t1, t2) =>
        for {
          a1 <- evR[Int](ctx, t1)
          a2 <- evR[Int](ctx, t2)
        } yield a1 >= a2
      case GT(t1, t2) =>
        for {
          a1 <- evR[Int](ctx, t1)
          a2 <- evR[Int](ctx, t2)
        } yield a1 > a2
      case i @ IF(cond, t1, t2) =>
        resolveType(ctx.defs, i).flatMap { tpe =>
          evR[Boolean](ctx, cond) flatMap {
            case true  => evR[tpe.Underlying](ctx, t1)
            case false => evR[tpe.Underlying](ctx, t2)
          }
        }
      case AND(t1, t2) =>
        evR[Boolean](ctx, t1) match {
          case Left(err)    => Left(err)
          case Right(false) => Right(false)
          case Right(true) =>
            evR[Boolean](ctx, t2) match {
              case Left(err) => Left(err)
              case Right(v)  => Right(v)
            }
        }
      case OR(t1, t2) =>
        evR[Boolean](ctx, t1) match {
          case Left(err)   => Left(err)
          case Right(true) => Right(true)
          case Right(false) =>
            evR[Boolean](ctx, t2) match {
              case Left(err) => Left(err)
              case Right(v)  => Right(v)
            }
        }
      case EQ_INT(it1, it2) =>
        for {
          i1 <- evR[Int](ctx, it1)
          i2 <- evR[Int](ctx, it2)
        } yield i1 == i2
      case SIG_VERIFY(msg, sig, pk) =>
        for {
          s <- evR[ByteVector](ctx, sig)
          m <- evR[ByteVector](ctx, msg)
          p <- evR[ByteVector](ctx, pk)
        } yield Curve25519.verify(s.toArray, m.toArray, p.toArray)
      case IS_DEFINED(opt) =>
        for {
          optType <- resolveType(ctx.defs, opt)
          optVal  <- evR[optType.Underlying](ctx, opt)
          res <- optVal match {
            case x: Option[_] =>
              x match {
                case Some(xx) => Right(true)
                case None     => Right(false)
              }
            case _ => Left("GET invoked on non-option type")
          }
        } yield res
      case GET(opt) =>
        for {
          optType <- resolveType(ctx.defs, opt)
          optVal  <- evR[optType.Underlying](ctx, opt)
          res <- optVal match {
            case x: Option[_] =>
              x match {
                case Some(xx) => Right(x)
                case None     => Left("None.get")
              }
            case _ => Left("GET invoked on non-option type")
          }
        } yield res
      // impure calls
      case HEIGHT => ctx.domain.height
      case TX_FIELD(f) =>
        f match {
          case Id        => ctx.domain.id
          case Type      => ctx.domain.tpe
          case SenderPk  => ctx.domain.senderPk
          case Proof(i)  => ctx.domain.proof(i)
          case BodyBytes => ctx.domain.bodyBytes
          case _         => ??? // match for  __satisfy_shapeless_0
        }
    }).map(_.asInstanceOf[T])

  def apply[A](c: Context, term: Expr): ExecResult[A] = {
    lazy val result = evR[A](c, term)
    Try(result) match {
      case Failure(ex)  => Left(ex.toString)
      case Success(res) => res
    }
  }
}
