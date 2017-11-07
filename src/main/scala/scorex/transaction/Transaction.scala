package scorex.transaction

import com.wavesplatform.state2._
import monix.eval.{Coeval, Task}
import monix.execution.schedulers.SchedulerService
import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.InvalidSignature

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait Transaction extends BytesSerializable with JsonSerializable with Signed {
  val id: Coeval[ByteStr]

  val transactionType: TransactionType.Value
  val assetFee: (Option[AssetId], Long)
  val timestamp: Long

  override def toString: String = json().toString()

  override def equals(other: Any): Boolean = other match {
    case tx: Transaction => id() == tx.id()
    case _ => false
  }

  override def hashCode(): Int = id().hashCode()
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
  protected val signatureValid: Coeval[Boolean]

  protected val signedDescendants: Coeval[Seq[Signed]] =Coeval.evalOnce(Seq.empty)

  protected val signaturesValidMemoized: Task[Either[InvalidSignature, this.type]] = Signed.validateTask[this.type](this).memoize

  val signaturesValid: Coeval[Either[InvalidSignature, this.type]] =  Coeval.evalOnce(Await.result(signaturesValidMemoized.runAsync(Signed.scheduler), Duration.Inf))
}

object Signed {

  type E[A] = Either[InvalidSignature, A]
  private implicit val scheduler: SchedulerService = monix.execution.Scheduler.computation(name = "sig-validator",
    reporter = com.wavesplatform.utils.UncaughtExceptionsToLogReporter)


  private def validateTask[S <: Signed](s: S): Task[E[S]] = Task {
    if (!s.signatureValid()) Task.now(Left(InvalidSignature(s, None)))
    else if (s.signedDescendants().isEmpty) Task.now(Right(s))
    else Task.wanderUnordered(s.signedDescendants())(s => s.signaturesValidMemoized) map { l =>
      l.find(_.isLeft) match {
        case Some(e) => Left(e.left.get)
        case None => Right(s)
      }
    }
  }.flatten

  def validateOrdered[S <: Signed](ss: Seq[S]): E[Seq[S]] = Await.result(
    Task.wander(ss)(s => s.signaturesValidMemoized).map { l =>
      l.find(_.isLeft) match {
        case Some(e) => Left(e.left.get)
        case None => Right(ss)
      }
    }.runAsync, Duration.Inf)

}
