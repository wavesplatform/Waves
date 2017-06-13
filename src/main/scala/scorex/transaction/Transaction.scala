package scorex.transaction

import cats._
import cats.data._
import cats.implicits._
import com.wavesplatform.state2.ByteStr
import scorex.block.Block
import scorex.network.Coordinator
import scorex.serialization.{BytesSerializable, JsonSerializable}
import scorex.transaction.TransactionParser.TransactionType
import scorex.transaction.ValidationError.InvalidSignature

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

trait Signed {
  protected def signatureValid: Boolean

  def signedDescendants: Seq[Signed] = Seq.empty
}

object Signed {

  type E[A] = Either[InvalidSignature, A]

  def validateSignature(s: Signed): Either[InvalidSignature, Signed] = for {
    v <- if (s.signatureValid) Right(s) else Left(InvalidSignature(s))
    ds <- Coordinator.foldM[E, List, Signed, Signed](s.signedDescendants.toList, v)
      { case (_, d) => validateSignature(d) }
  } yield s
}