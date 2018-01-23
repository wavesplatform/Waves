package scorex.transaction.smart.lang

import com.wavesplatform.Proofs
import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519
import scorex.transaction.ProvenTransaction
import scorex.transaction.smart.lang.Terms._

import scala.util.Try

object Evaluator {

  case class Context(height: Int, tx: ProvenTransaction)

  type ExcecutionError = String
  type EitherExecResult[T] = Either[ExcecutionError, T]

  def proofVal[T](proofs: Proofs, idx: Int): Either[ExcecutionError, T] =
    if (idx < proofs.proofs.size)
      Try(ByteVector(proofs.proofs(idx).arr).asInstanceOf[T]).toEither.left.map(_.toString)
    else Right(ByteVector.empty.asInstanceOf[T])

  def  apply[T](ctx: Context, t: Term[T]): EitherExecResult[T] = t match {
    case CONST_INT(v) => Right(v.asInstanceOf[T])
    case CONST_BYTEVECTOR(v) => Right(v.asInstanceOf[T])
    case SUM(t1, t2) => for {
      a1 <- apply(ctx, t1)
      a2 <- apply(ctx, t2)
    } yield (a1 + a2).asInstanceOf[T]
    case GE(t1, t2) => for {
      a1 <- apply(ctx, t1)
      a2 <- apply(ctx, t2)
    } yield (a1 >= a2).asInstanceOf[T]
    case GT(t1, t2) => for {
      a1 <- apply(ctx, t1)
      a2 <- apply(ctx, t2)
    } yield (a1 > a2).asInstanceOf[T]
    case IF(cond, t1, t2) => apply(ctx, cond) flatMap {
      case true => apply(ctx, t1)
      case false => apply(ctx, t2)
    }
    case AND(t1, t2) =>
      apply(ctx, t1) match {
        case Left(err) => Left(err)
        case Right(false) => Right(false.asInstanceOf[T])
        case Right(true) => apply(ctx, t2) match {
          case Left(err) => Left(err)
          case Right(v) => Right(v.asInstanceOf[T])
        }
      }
    case OR(t1, t2) =>
      apply(ctx, t1) match {
        case Left(err) => Left(err)
        case Right(true) => Right(true.asInstanceOf[T])
        case Right(false) => apply(ctx, t2) match {
          case Left(err) => Left(err)
          case Right(v) => Right(v.asInstanceOf[T])
        }
      }
    case HEIGHT => Right(ctx.height.asInstanceOf[T])
    case TX_FIELD(f) => f match {
      case Id => Right(ctx.tx.id().asInstanceOf[T])
      case Type => Right(ctx.tx.transactionType.id.asInstanceOf[T])
      case SenderPk => Right(ByteVector(ctx.tx.sender.publicKey).asInstanceOf[T])
      case Proof_0 => proofVal(ctx.tx.proofs, 0)
      case Proof_1 => proofVal(ctx.tx.proofs, 1)
      case Proof_2 => proofVal(ctx.tx.proofs, 2)
      case BodyBytes => Right(ByteVector(ctx.tx.bodyBytes()).asInstanceOf[T])
      case _ => ??? // match for  __satisfy_shapeless_0
    }
    case EQ_INT(it1, it2) => for {
      i1 <- apply(ctx, it1)
      i2 <- apply(ctx, it2)
    } yield (i1 == i2).asInstanceOf[T]
    case SIG_VERIFY(msg, sig, pk) => for {
      s <- apply(ctx, sig)
      m <- apply(ctx, msg)
      p <- apply(ctx, pk)
    } yield Curve25519.verify(s.toArray, m.toArray, p.toArray).asInstanceOf[T]
  }
}
