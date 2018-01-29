package scorex.transaction.smart.lang

import scodec.bits.ByteVector
import scorex.crypto.signatures.Curve25519
import scorex.transaction.{Proofs, ProvenTransaction}
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

  def apply[A](ctx: Context, term: Term): EitherExecResult[A] = {
    def r[T](t: Term): EitherExecResult[T] = (t match {
      case CONST_INT(v) => Right(v)
      case CONST_BYTEVECTOR(v) => Right(v)
      case SUM(t1, t2) => for {
        a1 <- r[Int](t1)
        a2 <- r[Int](t2)
      } yield a1 + a2
      case GE(t1, t2) => for {
        a1 <- r[Int](t1)
        a2 <- r[Int](t2)
      } yield a1 >= a2
      case GT(t1, t2) => for {
        a1 <- r[Int](t1)
        a2 <- r[Int](t2)
      } yield a1 > a2
      case IF(cond, t1, t2) => if (t1.tpe == t2.tpe)
        r[Boolean](cond) flatMap {
          case true => r(t1)
          case false => r(t2)
        } else Left("Bad types")
      case AND(t1, t2) =>
        r[Boolean](t1) match {
          case Left(err) => Left(err)
          case Right(false) => Right(false)
          case Right(true) => r[Boolean](t2) match {
            case Left(err) => Left(err)
            case Right(v) => Right(v)
          }
        }
      case OR(t1, t2) =>
        r[Boolean](t1) match {
          case Left(err) => Left(err)
          case Right(true) => Right(true)
          case Right(false) => r[Boolean](t2) match {
            case Left(err) => Left(err)
            case Right(v) => Right(v)
          }
        }
      case HEIGHT => Right(ctx.height)
      case TX_FIELD(f) => f match {
        case Id => Right(ctx.tx.id())
        case Type => Right(ctx.tx.transactionType.id)
        case SenderPk => Right(ByteVector(ctx.tx.sender.publicKey))
        case Proof_0 => proofVal(ctx.tx.proofs, 0)
        case Proof_1 => proofVal(ctx.tx.proofs, 1)
        case Proof_2 => proofVal(ctx.tx.proofs, 2)
        case BodyBytes => Right(ByteVector(ctx.tx.bodyBytes()))
        case _ => ??? // match for  __satisfy_shapeless_0
      }
      case EQ_INT(it1, it2) => for {
        i1 <- r[Int](it1)
        i2 <- r[Int](it2)
      } yield i1 == i2
      case SIG_VERIFY(msg, sig, pk) => for {
        s <- r[ByteVector](sig)
        m <- r[ByteVector](msg)
        p <- r[ByteVector](pk)
      } yield Curve25519.verify(s.toArray, m.toArray, p.toArray)
    }).map(_.asInstanceOf[T])

    r[A](term)
  }
}
