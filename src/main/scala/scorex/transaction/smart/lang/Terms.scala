package scorex.transaction.smart.lang

import com.wavesplatform.state2.ByteStr
import scorex.crypto.signatures.Curve25519
import scorex.transaction.ProvenTransaction
import scorex.transaction.smart.lang.Terms.Field._

object Terms {

  sealed trait Term[T]

  type BOOL = Term[Boolean]
  type INT = Term[Int]

  case class CONST[T](t: T) extends Term[T]

  type BYTEARRAY = Term[ByteStr]

  val FALSE = CONST(false)
  val TRUE = CONST(true)

  case class AND(t1: BOOL, t2: BOOL) extends BOOL
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
    case object Proof extends Field[ByteStr]

  }

  case class Accessor[T](tx: TX.type, f: Field[T]) extends Term[T]


  case class Context(height: Int, tx: ProvenTransaction)

  type ExcecutionError = String
  type EitherExecResult[T] = Either[ExcecutionError,T]

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
    case TX => Left("Nothing to do with TX")
    case HEIGHT => Right(ctx.height.asInstanceOf[T])
    case Accessor(_, f) => f match {
      case Id => Right(ctx.tx.id().asInstanceOf[T])
      case Type => Right(ctx.tx.transactionType.id.asInstanceOf[T])
      case SenderPk => Right(ByteStr(ctx.tx.sender.publicKey).asInstanceOf[T])
      case Proof =>  Right(ctx.tx.proof.asInstanceOf[T])
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
