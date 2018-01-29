package scorex.transaction.smart.lang

import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519
import scorex.transaction.smart.lang.Terms._
import scorex.transaction.{Proofs, ProvenTransaction}

import scala.util.{Failure, Success, Try}

object Evaluator {

  case class Context(height: Int, tx: ProvenTransaction, defs: Map[String, (Type, Any)]) {

    private def typeof(t: Expr): Option[Type] = t match {
      case ref: REF => defs.get(ref.key).map(_._1)
      case x        => x.knownType
    }

    def resolveType(t: Expr): Either[ExcecutionError, Type] = typeof(t) match {
      case None           => Left(s"Cannot resolve type of $t")
      case Some(tt: Type) => Right(tt)
    }
  }

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
            case None => ctx.resolveType(inner).flatMap(termType => r[termType.Underlying](ctx, inner))
            case Some(LET(newVarName: String, newVarExpr: CExpr)) =>
              for {
                newVarType  <- ctx.resolveType(newVarExpr)
                newVarValue <- r[newVarType.Underlying](ctx, newVarExpr)
                _           <- Either.cond(ctx.defs.get(newVarName).isEmpty, (), s"Value '$newVarName' already defined in the scope")
                newCtx = ctx.copy(defs = ctx.defs + (newVarName -> (newVarType, newVarValue)))
                termType <- newCtx.resolveType(inner)
                res      <- r[termType.Underlying](newCtx, inner)
              } yield res
          }
        case LET(name, v) => ctx.resolveType(v).flatMap(rType => r[rType.Underlying](ctx, v))
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
        case IF(cond, t1, t2) =>
          for {
            tt1 <- ctx.resolveType(t1)
            tt2 <- ctx.resolveType(t2)
            _   <- Either.cond(tt1 == tt2, (), "Type mismatch")
            x <- r[Boolean](ctx, cond) flatMap {
              case true  => ctx.resolveType(t1).flatMap(t1type => r[t1type.Underlying](ctx, t1))
              case false => ctx.resolveType(t2).flatMap(t2type => r[t2type.Underlying](ctx, t2))
            }
          } yield x
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
