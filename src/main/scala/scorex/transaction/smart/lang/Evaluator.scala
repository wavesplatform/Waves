package scorex.transaction.smart.lang

import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519
import scorex.transaction.smart.lang.Terms._
import scorex.transaction.{Proofs, ProvenTransaction}

import scala.util.{Failure, Success, Try}

object Evaluator {

  type Defs = Map[String, (Type, Any)]

  case class Context(height: Int, tx: ProvenTransaction, defs: Defs)

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
        case x          => Left(s"Typecheck failed: GET called on $x, but only call on OPTION is allowed")
      }
    case x => Right(x.predefinedType.get)
  }

  type TypeResolutionError = String
  type ExcecutionError     = String
  type EitherExecResult[T] = Either[ExcecutionError, T]

  def proofVal[T](proofs: Proofs, idx: Int): Either[ExcecutionError, T] =
    if (idx < proofs.proofs.size)
      Try(ByteVector(proofs.proofs(idx).arr).asInstanceOf[T]).toEither.left.map(_.toString)
    else Right(ByteVector.empty.asInstanceOf[T])

  def apply[A](c: Context, term: Expr): EitherExecResult[A] = {

    def r[T](ctx: Context, t: Expr): EitherExecResult[T] =
      (t match {
        case CExpr(maybelet, inner) =>
          maybelet match {
            case None => resolveType(ctx.defs, inner).flatMap(termType => r[termType.Underlying](ctx, inner))
            case Some(LET(newVarName: String, newVarExpr: CExpr)) =>
              for {
                newVarType  <- resolveType(ctx.defs, newVarExpr)
                newVarValue <- r[newVarType.Underlying](ctx, newVarExpr)
                _           <- Either.cond(ctx.defs.get(newVarName).isEmpty, (), s"Value '$newVarName' already defined in the scope")
                newDefs = ctx.defs + (newVarName -> (newVarType, newVarValue))
                termType <- resolveType(newDefs, inner)
                res      <- r[termType.Underlying](ctx.copy(defs = newDefs), inner)
              } yield res
          }
        case LET(name, v) => resolveType(ctx.defs, v).flatMap(rType => r[rType.Underlying](ctx, v))
        case REF(str) =>
          ctx.defs.get(str) match {
            case Some((x, y)) => Right(y.asInstanceOf[x.Underlying])
            case None         => Left(s"Definition '$str' not found")
          }
        case CONST_INT(v)        => Right(v)
        case CONST_BYTEVECTOR(v) => Right(v)
        case SUM(t1, t2) =>
          for {
            a1 <- r[Int](ctx, t1)
            a2 <- r[Int](ctx, t2)
          } yield a1 + a2
        case GE(t1, t2) =>
          for {
            a1 <- r[Int](ctx, t1)
            a2 <- r[Int](ctx, t2)
          } yield a1 >= a2
        case GT(t1, t2) =>
          for {
            a1 <- r[Int](ctx, t1)
            a2 <- r[Int](ctx, t2)
          } yield a1 > a2
        case i @ IF(cond, t1, t2) =>
          resolveType(ctx.defs, i).flatMap { _ =>
            r[Boolean](ctx, cond) flatMap {
              case true  => r(ctx, t1)
              case false => r(ctx, t2)
            }
          }
        case AND(t1, t2) =>
          r[Boolean](ctx, t1) match {
            case Left(err)    => Left(err)
            case Right(false) => Right(false)
            case Right(true) =>
              r[Boolean](ctx, t2) match {
                case Left(err) => Left(err)
                case Right(v)  => Right(v)
              }
          }
        case OR(t1, t2) =>
          r[Boolean](ctx, t1) match {
            case Left(err)   => Left(err)
            case Right(true) => Right(true)
            case Right(false) =>
              r[Boolean](ctx, t2) match {
                case Left(err) => Left(err)
                case Right(v)  => Right(v)
              }
          }
        case HEIGHT => Right(ctx.height)
        case TX_FIELD(f) =>
          f match {
            case Id        => Right(ctx.tx.id())
            case Type      => Right(ctx.tx.transactionType.id)
            case SenderPk  => Right(ByteVector(ctx.tx.sender.publicKey))
            case Proof_0   => proofVal(ctx.tx.proofs, 0)
            case Proof_1   => proofVal(ctx.tx.proofs, 1)
            case Proof_2   => proofVal(ctx.tx.proofs, 2)
            case BodyBytes => Right(ByteVector(ctx.tx.bodyBytes()))
            case _         => ??? // match for  __satisfy_shapeless_0
          }
        case EQ_INT(it1, it2) =>
          for {
            i1 <- r[Int](ctx, it1)
            i2 <- r[Int](ctx, it2)
          } yield i1 == i2
        case SIG_VERIFY(msg, sig, pk) =>
          for {
            s <- r[ByteVector](ctx, sig)
            m <- r[ByteVector](ctx, msg)
            p <- r[ByteVector](ctx, pk)
          } yield Curve25519.verify(s.toArray, m.toArray, p.toArray)
      }).map(_.asInstanceOf[T])

    lazy val result = r[A](c, term)
    Try(result) match {
      case Failure(ex)  => Left(ex.toString)
      case Success(res) => res
    }

  }
}
