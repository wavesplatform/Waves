package scorex.transaction

import com.wavesplatform.state2.ByteStr
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

  def validateSignatures[S <: Signed](s: S): E[S] =
    if (!s.signatureValid) Left(InvalidSignature(s, None))
    else s.signedDescendants.par.find { descendant =>
      validateSignatures(descendant).isLeft
    }.fold[E[S]](Right(s))(sd => Left(InvalidSignature(s, Some(validateSignatures(sd).left.get))))
}
