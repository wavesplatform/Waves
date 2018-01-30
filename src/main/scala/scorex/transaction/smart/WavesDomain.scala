package scorex.transaction.smart

import com.wavesplatform.lang.Domain
import com.wavesplatform.lang.Evaluator.{ExcecutionError, ExecResult}
import scodec.bits.ByteVector
import scorex.transaction.{Proofs, ProvenTransaction}

import scala.util.Try

class WavesDomain(h: Int, tx: ProvenTransaction) extends Domain {
  override def Height: ExecResult[Int]           = Right(h)
  override def Id: ExecResult[ByteVector]        = Right(ByteVector(tx.id().arr))
  override def Type: ExecResult[Int]             = Right(tx.transactionType.id)
  override def SenderPk: ExecResult[ByteVector]  = Right(ByteVector(tx.sender.publicKey))
  override def BodyBytes: ExecResult[ByteVector] = Right(ByteVector(tx.bodyBytes()))
  override def Proof_0: ExecResult[ByteVector]   = proofVal(tx.proofs, 0)
  override def Proof_1: ExecResult[ByteVector]   = proofVal(tx.proofs, 1)
  override def Proof_2: ExecResult[ByteVector]   = proofVal(tx.proofs, 2)

  def proofVal[T](proofs: Proofs, idx: Int): Either[ExcecutionError, T] =
    if (idx < proofs.proofs.size)
      Try(ByteVector(proofs.proofs(idx).arr).asInstanceOf[T]).toEither.left.map(_.toString)
    else Right(ByteVector.empty.asInstanceOf[T])

}
