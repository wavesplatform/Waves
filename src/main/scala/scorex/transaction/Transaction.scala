package scorex.transaction

import com.wavesplatform.state2._
import monix.eval.Task
import monix.execution.schedulers.SchedulerService
import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.InvalidSignature

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait Transaction extends BytesSerializable with JsonSerializable with Signed {
  val id: ByteStr

  val transactionType: TransactionType.Value
  val assetFee: (Option[AssetId], Long)
  val timestamp: Long

  override def toString: String = json.toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id == tx.id
    case _ => false
  }

  override def hashCode(): Int = id.hashCode()
}

object Transaction {

  implicit class TransactionExt(tx: Transaction) {
    def feeDiff(): Portfolio = tx.assetFee match {
      case (Some(asset), fee) =>
        Portfolio(
          balance = 0,
          leaseInfo = LeaseInfo.empty,
          assets = Map(asset -> fee))
      case (None, fee) => Portfolio(
        balance = fee,
        leaseInfo = LeaseInfo.empty,
        assets = Map.empty)
    }
  }

}


trait Signed {
  protected def signatureValid: Boolean

  protected def signedDescendants: Seq[Signed] = Seq.empty

  val signaturesValid: Either[InvalidSignature, this.type] = Signed.validateSignatures(this)
}

object Signed {

  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.computation(name = "sig-validator",
    reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)

  type E[A] = Either[InvalidSignature, A]

  def validateSignatures[S <: Signed](s: S): E[S] = Await.result(Signed.validateTask(s).runAsync, Duration.Inf)

  private def validateTask[S <: Signed](s: S): Task[E[S]] =
    if (!s.signatureValid) Task.now(Left(InvalidSignature(s, None)))
    else if (s.signedDescendants.isEmpty) Task.now(Right(s))
    else Task.wanderUnordered(s.signedDescendants)(s => validateTask(s)) map { l =>
      l.find(_.isLeft) match {
        case Some(e) => Left(e.left.get)
        case None => Right(s)
      }
    }
}
