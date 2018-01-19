package scorex.transaction.smart.lang

import com.wavesplatform.state2.ByteStr
import scorex.crypto.signatures.Curve25519
import scorex.transaction.ProvenTransaction
import scorex.transaction.smart.lang.Terms.Field._

import scala.util.Try

object Terms {

  sealed trait Term[T]

  type BOOL = Term[Boolean]
  type INT = Term[Int]

  case class CONST[T](t: T) extends Term[T]

  type BYTEARRAY = Term[ByteStr]

  val FALSE = CONST(false)
  val TRUE = CONST(true)

  case class SUM(i1: INT, i2: INT) extends INT

  case class IF[T](cond: BOOL, ifTrue: Term[T], ifFalse: Term[T]) extends Term[T]

  case class AND(t1: BOOL, t2: BOOL) extends BOOL

  case class OR(t1: BOOL, t2: BOOL) extends BOOL

  case class EQINT(t1: INT, t2: INT) extends BOOL

  case class SIGVERIFY(message: BYTEARRAY, signature: BYTEARRAY, publicKey: BYTEARRAY) extends BOOL

  case object HEIGHT extends INT

  object Transaction

  case object TX extends Term[Transaction.type]

  sealed trait Field[T]

  object Field {

    case object Id extends Field[ByteStr]

    case object Type extends Field[Int]

    case object SenderPk extends Field[ByteStr]

    case object BodyBytes extends Field[ByteStr]

    case object Proof_0 extends Field[ByteStr]

    case object Proof_1 extends Field[ByteStr]

    case object Proof_2 extends Field[ByteStr]

    case object Proof_3 extends Field[ByteStr]

    case object Proof_4 extends Field[ByteStr]

    case object Proof_5 extends Field[ByteStr]

    case object Proof_6 extends Field[ByteStr]

    case object Proof_7 extends Field[ByteStr]

  }

  case class Accessor[T](tx: TX.type, f: Field[T]) extends Term[T]


  case class Context(height: Int, tx: ProvenTransaction)

  type ExcecutionError = String
  type EitherExecResult[T] = Either[ExcecutionError, T]

  def proofVal[T](proofs: Seq[ByteStr], idx: Int) = Try(proofs(idx).asInstanceOf[T]).toEither.left.map(_.toString)

  def eval[T](ctx: Context, t: Term[T]): EitherExecResult[T] = t match {
    case CONST(v) => Right(v)
    case AND(t1, t2) =>
      eval(ctx, t1) match {
        case Left(err) => Left(err)
        case Right(false) => Right(false.asInstanceOf[T])
        case Right(true) => eval(ctx, t2) match {
          case Left(err) => Left(err)
          case Right(v) => Right(v.asInstanceOf[T])
        }
      }
    case OR(t1, t2) =>
      eval(ctx, t1) match {
        case Left(err) => Left(err)
        case Right(true) => Right(true.asInstanceOf[T])
        case Right(false) => eval(ctx, t2) match {
          case Left(err) => Left(err)
          case Right(v) => Right(v.asInstanceOf[T])
        }
      }
    case TX => Left("Nothing to do with TX")
    case HEIGHT => Right(ctx.height.asInstanceOf[T])
    case Accessor(_, f) => f match {
      case Id => Right(ctx.tx.id().asInstanceOf[T])
      case Type => Right(ctx.tx.transactionType.id.asInstanceOf[T])
      case SenderPk => Right(ByteStr(ctx.tx.sender.publicKey).asInstanceOf[T])
      case Proof_0 => proofVal(ctx.tx.proofs, 0)
      case Proof_1 => proofVal(ctx.tx.proofs, 1)
      case Proof_2 => proofVal(ctx.tx.proofs, 2)
      case Proof_3 => proofVal(ctx.tx.proofs, 3)
      case Proof_4 => proofVal(ctx.tx.proofs, 4)
      case Proof_5 => proofVal(ctx.tx.proofs, 5)
      case Proof_6 => proofVal(ctx.tx.proofs, 6)
      case Proof_7 => proofVal(ctx.tx.proofs, 7)
      case BodyBytes => Right(ByteStr(ctx.tx.bodyBytes()).asInstanceOf[T])
    }
    case EQINT(it1, it2) => for {
      i1 <- eval(ctx, it1)
      i2 <- eval(ctx, it2)
    } yield (i1 == i2).asInstanceOf[T]
    case SIGVERIFY(msg, sig, pk) => for {
      s <- eval(ctx, sig)
      m <- eval(ctx, msg)
      p <- eval(ctx, pk)
    } yield Curve25519.verify(s.arr, m.arr, p.arr).asInstanceOf[T]
  }
}
