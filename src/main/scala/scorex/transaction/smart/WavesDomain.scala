package scorex.transaction.smart

import com.wavesplatform.lang.Domain
import com.wavesplatform.lang.Evaluator.ExecResult
import scodec.bits.ByteVector
import scorex.transaction.ProvenTransaction

import scala.util.Try

class WavesDomain(h: Int, tx: ProvenTransaction) extends Domain {
  override def height: ExecResult[Int]           = Right(h)
  override def id: ExecResult[ByteVector]        = Right(ByteVector(tx.id().arr))
  override def tpe: ExecResult[Int]              = Right(tx.transactionType.id)
  override def senderPk: ExecResult[ByteVector]  = Right(ByteVector(tx.sender.publicKey))
  override def bodyBytes: ExecResult[ByteVector] = Right(ByteVector(tx.bodyBytes()))
  override def proof(idx: Int): ExecResult[ByteVector] =
    if (idx < tx.proofs.proofs.size)
      Try(ByteVector(tx.proofs.proofs(idx).arr)).toEither.left.map(_.toString)
    else Right(ByteVector.empty)

}
